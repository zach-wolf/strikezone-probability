library(baseballr)
library(tidyverse)
library(tidymodels)
library(DBI)
library(RPostgreSQL)
library(RPostgres)
library(data.table)
library(ggplot2)
library(xgboost)

## Load and create necessary objects
statcast_db <- dbConnect(RPostgres::Postgres(), dbname = "statcast")

xgb_fit <- readRDS("Modeling//strike_prob_xgb_tuned.Rds")
pitch_data <- read_csv("Data//pitch_data.csv")

factor_vars <- c("pitch_type", "p_throws", "stand", "inning_topbot", "runners_on", "risp", "pitch_group", "IsStrike")

num_vars <- unlist(lapply(pitch_data, is.numeric)) 
num_vars_nm <- colnames(pitch_data[,num_vars])

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.05, .95), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

## 2021 pitches
pitch_dat_2021 <- tbl(statcast_db, 'statcast') %>%
  filter(game_date > '2021-01-01', 
         game_type == 'R') %>%
  collect() %>%
  select(fielder_2, game_date, pitch_type, release_speed, release_spin_rate, spin_axis, release_extension, release_pos_x, release_pos_y, release_pos_z, pfx_x, pfx_z, p_throws, stand, sz_top, sz_bot, balls, strikes, inning, inning_topbot, plate_x, plate_z, on_3b, on_2b, on_1b, description) %>%
  mutate(runners_on = case_when(!is.na(on_1b) | !is.na(on_2b) | !is.na(on_3b) ~ 1,
                                TRUE ~ 0),
         risp = case_when(!is.na(on_2b) | !is.na(on_3b) ~ 1,
                          TRUE ~ 0),
         pitch_group = case_when(pitch_type %in% c("FF","FT","SI","FA") ~ "Fastball",
                                 pitch_type %in% c("SL","FC","CU") ~ "Breaking",
                                 pitch_type %in% c("CH","FS","FO","SC") ~ "Off-Speed",
                                 TRUE ~ "Other"),
         IsStrike = case_when(description == "called_strike" ~ 1,
                              description %in% c("ball", "blocked_ball") ~ 0,
                              TRUE ~ 2)) %>%
  filter(IsStrike != 2) %>%
  select(-c(on_3b, on_2b, on_1b, description)) %>%
  group_by(pitch_type) %>%
  mutate_at(all_of(num_vars_nm), funs(remove_outliers)) %>%
  ungroup() 
pitch_dat_2021[factor_vars] <- lapply(pitch_dat_2021[factor_vars], factor)
pitch_dat_2021 <- pitch_dat_2021[complete.cases(pitch_dat_2021),]

pitch_pred_2021 <- xgb_fit %>%
  predict(pitch_dat_2021, type = "prob") %>%
  bind_cols(pitch_dat_2021)

## Player names to match to player ids
playerid_mapping <- read.csv("Data//playerid_mapping.csv")

## 2021 Catcher Receiving Rankings
rankings <- pitch_pred_2021 %>%
  mutate(strikes_plus_minus = as.numeric(as.character(IsStrike)) - .pred_1) %>%
  group_by(fielder_2) %>%
  summarise(n_pitch = n(),
            games = length(unique(game_date)),
            strikes_plus_minus = sum(strikes_plus_minus)) %>%
  mutate(strikes_pm_per_pitch = strikes_plus_minus / n_pitch,
         strikes_pm_per_game = strikes_plus_minus / n_pitch * 70, # 70 pitches per game
         strikes_pm_per_100game = strikes_plus_minus / n_pitch * 7000) %>%
  arrange(desc(strikes_pm_per_pitch)) %>%
  filter(games >= 25) %>% # filter to only catchers with >= 50 games at catcher
  left_join(playerid_mapping %>% select(MLBID, PLAYERNAME, TEAM), by = c("fielder_2" = "MLBID")) %>%
  mutate(PLAYERNAME = as.character(PLAYERNAME),
         TEAM = as.character(TEAM)) %>%
  rename(mlb_id = fielder_2,
         player_name = PLAYERNAME,
         team = TEAM) %>%
  select(mlb_id, player_name, team, games, n_pitch, strikes_plus_minus, strikes_pm_per_pitch, strikes_pm_per_game, strikes_pm_per_100game) %>%
  mutate(player_name = case_when(mlb_id == 666163 ~ "Ben Rortvedt",
                                 TRUE ~ player_name),
         team = case_when(mlb_id == 666163 ~ "MIN",
                          TRUE ~ team)) # Rortvedt not mapped in mapping csv

# write.csv(rankings, "Scoring//2021_catcher_receiving_rankings.csv", row.names = FALSE)

team_rankings <- rankings %>%
  group_by(team) %>%
  summarise_at(vars(strikes_pm_per_pitch:strikes_pm_per_100game), 
               funs(weighted.mean(., w = n_pitch, na.rm = TRUE)))
