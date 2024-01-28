if (!require("remotes")) install.packages("remotes")
remotes::install_github("SCasanova/f1dataR")
library(f1dataR)
library(dplyr)
library(xgboost)
library(caret)
library(vip)
library(gt)
library(gtExtras)
library(ggplot2)
library(ggpath)

turns <- data.frame()
for (szn in 2021:2023) {
  for(round in 1:22) {
    if (!(szn == 2021 & round == 12)) {
      details <- load_circuit_details(szn, round)
      specific_details <- as.data.frame(details[[1]])
      specific_details$season <- szn
      specific_details$round <- round
      turns <- rbind(turns, specific_details)  
    }
  }
}

telemetry <- data.frame()
for (szn in 2021:2023) {
  for(round in 1:22) {
    drivers <- load_drivers(szn)
    for(driver in drivers$code) {
      tryCatch({
        if (!(szn == 2021 & round == 12)) {
          specific_telemetry <- load_driver_telemetry(szn, round, driver = driver)
          telemetry <- rbind(telemetry, specific_telemetry)
        }
      }, error = function(e) {
        cat("Error for ", szn, " ", round, " ", driver, "\n")
      })
    }
  }
}

telemetry$year <- as.numeric(format(telemetry$date, "%Y"))
telemetry$date <- as.Date(telemetry$date)

schedules_21 <- load_schedule(2021) %>% select(date, round)
schedules_22 <- load_schedule(2022) %>% select(date, round)
schedules_23 <- load_schedule(2023) %>% select(date, round)

schedules <- rbind(schedules_21, schedules_22, schedules_23)

schedules$date <- as.Date(schedules$date)
final_telemetry <- left_join(telemetry, schedules, by = "date")

# Custom function for Euclidean distance
euclidean_distance <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

final_telemetry$round <- as.integer(final_telemetry$round)

# Left join based on minimized Euclidean distance
result_df <- final_telemetry %>%
  mutate(telem_row_num = row_number()) %>%
  left_join(turns, by = c("year"="season", "round"), suffix = c("_telem", "_turn")) %>%
  rowwise() %>%
  mutate(distance = euclidean_distance(x_telem, y_telem, x_turn, y_turn)) %>%
  arrange(distance) %>%
  slice(1) %>%
  ungroup()

final <- result_df %>%
  group_by(telem_row_num) %>%
  filter(distance == min(distance)) %>%
  ungroup() %>%
  group_by(driver_code, year, round, number) %>%
  mutate(turn = ifelse(distance == min(distance), 1, 0)) %>%
  ungroup()

final <- final %>% arrange(telem_row_num)

before_stats <- final %>%
  mutate(before = cumsum(turn == 1)) %>%
  group_by(year, round, driver_code, before) %>%
  summarize(
    data_since_last_turn = n(),
    avg_speed_since_last_turn = mean(speed)
  ) %>%
  ungroup()

final <- final %>%
  mutate(before = cumsum(turn == 1)) %>%
  left_join(before_stats, by = c('year', 'round', 'driver_code', 'before')) 

final$distance_to_driver_ahead[which(is.nan(final$distance_to_driver_ahead))] <- 0

turn_data <- final %>% filter(turn == 1, avg_speed_since_last_turn != 0) 

turn_data <- turn_data %>%
  select(year, round, driver_code, speed, time, distance_to_driver_ahead, angle, data_since_last_turn, avg_speed_since_last_turn)

xgboost_train <- turn_data %>%
  filter(year < 2023)

str(xgboost_train)

xgboost_test <- turn_data %>%
  filter(year >= 2023)

labels_train <- as.matrix(xgboost_train[, 4])
xgboost_trainfinal <- as.matrix(xgboost_train[, c(5:9)])
xgboost_testfinal <- as.matrix(xgboost_test[, c(5:9)])

tsoe_model <- xgboost(data = xgboost_trainfinal, label = labels_train, nrounds = 100, objective = "reg:squarederror", early_stopping_rounds = 10, max_depth = 6, eta = 0.3)

vip(tsoe_model)

turn_speed_predict <- predict(tsoe_model, xgboost_testfinal)
turn_speed <- as.matrix(xgboost_test[,4])
postResample(turn_speed_predict, turn_speed)

turn_speed_predictions <- as.data.frame(
  matrix(predict(tsoe_model, as.matrix(turn_data[,c(5:9)])))
)

all_stats <- cbind(turn_data, turn_speed_predictions) %>%
  select(year, round, driver_code, turn_speed = speed, pred_turn_speed = V1)

all_stats <- all_stats %>%
  group_by(year, driver_code) %>%
  summarize(avg_turn_speed = mean(turn_speed), avg_pred_turn_speed = mean(pred_turn_speed), tsoe = avg_turn_speed - avg_pred_turn_speed)

stats_2023 <- all_stats %>% filter(year == 2023)

standings <- load_standings(2023) 

drivers <- load_drivers(2023) %>%
  mutate(name = paste(given_name, family_name)) %>%
  left_join(standings, by = "driver_id")

constructors <- load_constructors()

drivers <- drivers %>%
  left_join(constructors, by = "constructor_id")

colnames(drivers)[colnames(drivers) == 'name.y'] <- 'constructor'

drivers$driver_id[which(drivers$driver_id == "de_vries")] <- "devries"
drivers$driver_id[which(drivers$driver_id == "kevin_magnussen")] <- "magnussen"
drivers$driver_id[which(drivers$driver_id == "max_verstappen")] <- "verstappen"

drivers$constructor_id[which(drivers$constructor_id == "alfa")] <- "alfa%20romeo"
drivers$constructor_id[which(drivers$constructor_id == "aston_martin")] <- "aston%20martin"
drivers$constructor_id[which(drivers$constructor_id == "red_bull")] <- "red%20bull"

stats_2023 <- stats_2023 %>%
  left_join(drivers, by = c("driver_code"="code"))

stats_2023
gt_nice_stats <- stats_2023 %>%
  mutate(headshot_link = paste0("https://media.formula1.com/content/dam/fom-website/drivers/2023Drivers/", driver_id, ".jpg.img.1536.medium.jpg/1701270073824.jpg")) %>%
  mutate(team_logo = paste0("https://media.formula1.com/content/dam/fom-website/2018-redesign-assets/team%20logos/", constructor_id, ".jpg")) %>%
  arrange(-tsoe) %>%
  mutate(tsoe = round(tsoe, 3)) %>%
  ungroup() %>%
  select(headshot_link, name = name.x, team_logo, points, tsoe) 

gt_nice_stats$headshot_link[which(gt_nice_stats$name == "Liam Lawson")] <- "https://motorsportmagazine.b-cdn.net/database/wp-content/uploads/sites/2/2020/12/Liam-Lawson-portrait-square-200x200.jpg"
gt_nice_stats$points <- as.numeric(gt_nice_stats$points)

gt_align_caption <- function(left, right) {
  caption <- paste0(
    '<span style="float: left;">', left, '</span>',
    '<span style="float: right;">', right, '</span>'
  )
  return(caption)
}

caption = gt_align_caption("Data from <b>f1dataR</b>", "Amrit Vignesh")

nice_table <- gt_nice_stats %>% gt() %>%
  gt_img_rows(columns = team_logo, height = 64) %>%
  gt_img_rows(columns = headshot_link, height = 80) %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = c(headshot_link, name, team_logo, points, tsoe)
  ) %>%
  gt_hulk_col_numeric(c(points, tsoe)) %>%
  cols_label(
    headshot_link = md(""),
    name = md("**Driver**"),
    team_logo = md("**Team**"),
    points = md("**Points**"),
    tsoe = md("**TSOE**")
  ) %>%
  tab_header(
    title = "2023 Formula One TSOE (Turn Speed Over Expected)",
    subtitle = md("*TSOE In **KM/H***")
  ) %>% 
  tab_source_note(html(caption)) %>%
  opt_align_table_header(align = "center") %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(name, tsoe)
    )
  ) %>%
  cols_width(name ~ px(220), headshot_link ~ px(110))

gtsave(nice_table, "nice_table.png", vwidth = 1000, vheight = 2500, zoom = 1)

plot <- gt_nice_stats %>%
  ggplot(aes(x = tsoe, y = points)) +
  geom_hline(yintercept = mean(gt_nice_stats$points), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = mean(gt_nice_stats$tsoe), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_smooth(method = "lm") + 
  geom_from_path(aes(x = tsoe, y = points, path = headshot_link), width = 0.1, height = 0.1) +
  labs(x = "TSOE",
       y = "Points",
       title = "Ability to Manage Turns in Relation to Points",
       caption = "Amrit Vignesh") + 
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) 

ggsave("plot.png", plot, width = 12)


