library(baseballr)
library(tidyverse)
library(DBI)
library(RPostgreSQL)
library(RPostgres)
library(data.table)
library(ggplot2)

#### DATA ####

# statcast db was built with statcast_scrape.R script in the Data folder
statcast_db <- dbConnect(RPostgres::Postgres(), dbname = "statcast")

tbl(statcast_db, 'statcast') %>%
  group_by(game_year) %>%
  count() %>%
  collect()

head(pitch_data)

all_data <- tbl(statcast_db, 'statcast') %>%
  filter(game_date >= '2017-01-01', # first year of Trackman
         # game_date < '2021-01-01',
         game_type == 'R',
         plate_x >= -2,
         plate_x <= 2,
         plate_z >= 0,
         plate_z <= 5) %>%
  collect()

gc()

pitch_data <- all_data %>%
  select(pitch_type, release_speed, release_spin_rate, spin_axis, release_extension, release_pos_x, release_pos_y, release_pos_z, pfx_x, pfx_z, p_throws, stand, sz_top, sz_bot, balls, strikes, inning, inning_topbot, plate_x, plate_z, on_3b, on_2b, on_1b, description) %>%
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
  select(-c(on_3b, on_2b, on_1b, description)) 

pitch_data <- pitch_data[complete.cases(pitch_data),]

factor_vars <- c("pitch_type", "p_throws", "stand", "inning_topbot", "runners_on", "risp", "pitch_group", "IsStrike")
pitch_data[factor_vars] <- lapply(pitch_data[factor_vars], factor)

# outliers
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

pitch_data2 <- pitch_data %>%
  group_by(pitch_type) %>%
  mutate_at(all_of(colnames(pitch_data[,num_vars])), funs(remove_outliers)) %>%
  ungroup()

pitch_data2 <- pitch_data2[complete.cases(pitch_data2),]
# write.csv(pitch_data2[sample(1:nrow(pitch_data2), 750000),], "Data//pitch_data.csv", row.names = FALSE)
rm(all_data)
rm(pitch_data)
gc()

## FEATURES CONSIDERED
# Pitch Type
# Velocity
# Spin Rate
# Spin Axis
# Extension
# Release Position X
# Release Position Y
# Release Position Z
# Horizontal Break
# Vertical Break
# Pitcher Handedness
# Batter Stance
# Strikezone top (adj for batter)
# Strikezone bottom (adj for batter)
# Balls
# Strikes
# Inning
# Inning Top/Bottom (control for home/away)
# Plate X
# Plate Z

#### BUILDING MODEL ####

library(tidymodels)

set.seed(123)
pitch_split <- initial_split(pitch_data2, strata = IsStrike)
pitch_train <- training(pitch_split)
pitch_test <- testing(pitch_split)

pitch_recipe <- recipe(IsStrike ~ ., data = pitch_train) %>%
  step_dummy(all_of(factor_vars), -IsStrike, one_hot = FALSE)

# xgboost model without tuning
xgb_mod <- boost_tree(
  trees = 500,
  mode = "classification"
) %>% 
  set_engine("xgboost", nthread = 8)

xgb_wf <- workflow() %>%
  add_model(xgb_mod) %>%
  add_recipe(pitch_recipe)

start <- Sys.time()
xgb_fit <- xgb_wf %>%
  fit(data = pitch_train)
Sys.time() - start # 19 minutes

xgb_fit %>%
  extract_fit_parsnip()

xgb_pred <-
  predict(xgb_fit, pitch_test, type = "prob") %>%
  bind_cols(pitch_test)

xgb_pred %>%
  roc_auc(truth = IsStrike, .pred_0) # 0.982

xgb_fit %>%
  extract_fit_parsnip() %>%
  vip::vip(geom = "point", n = 20)

xgb_pred %>%
  # filter(.pred_1 > 0.45 & .pred_1 < 0.55) %>%
  ggplot(aes(x = plate_x, y = plate_z, color = .pred_1)) +
  geom_raster(aes(fill = .pred_1), interpolate = TRUE) +
  scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF","#FF0000FF")) +
  geom_segment(x = 0.75, y = mean(xgb_pred$sz_bot), xend = 0.75, yend = mean(xgb_pred$sz_top), color = "black") +
  geom_segment(x = -0.75, y = mean(xgb_pred$sz_bot), xend = -0.75, yend = mean(xgb_pred$sz_top), color = "black") +
  geom_segment(x = 0.75, y = mean(xgb_pred$sz_bot), xend = -0.75, yend = mean(xgb_pred$sz_bot), color = "black") +
  geom_segment(x = 0.75, y = mean(xgb_pred$sz_top), xend = -0.75, yend = mean(xgb_pred$sz_top), color = "black") +
  coord_fixed()

# xgboost model with tuning
xgb_mod <- 
  boost_tree(
    trees = 250, 
    tree_depth = tune(), 
    min_n = tune(), 
    loss_reduction = tune(),                     ## first three: model complexity
    learn_rate = tune()                          ## step size
  ) %>% 
  set_engine("xgboost", nthread = 8) %>% 
  set_mode("classification")

xgb_wf <- 
  workflow() %>%
  add_recipe(pitch_recipe) %>%
  add_model(xgb_mod)

xgb_params <-
  parameters(
    tree_depth(),
    min_n(),
    loss_reduction(),
    learn_rate()
  )

xgb_grid <- 
  grid_max_entropy(
    xgb_params,
    size = 20
  )

perf_metrics <- metric_set(roc_auc, ppv, npv, mn_log_loss)
ctrl_grid <- control_grid(save_pred = TRUE, save_workflow = TRUE, verbose = TRUE)

set.seed(123)
pitch_folds <- vfold_cv(pitch_train, strata = IsStrike, v = 3)

start <- Sys.time()
set.seed(234)
xgb_tuned <- 
  xgb_wf %>%
  tune_grid(
    resamples = pitch_folds,
    grid = xgb_grid,
    metrics = perf_metrics,
    control = ctrl_grid
  )
Sys.time() - start # 9.5 hours

xgb_tuned %>% show_best(metric = "roc_auc", n = 10)

best_xgb <- xgb_tuned %>% select_best(metric = "roc_auc")

final_xgb <- finalize_workflow(xgb_wf, best_xgb)
xgb_fit <- final_xgb %>% fit(pitch_train)

vip_plot <- xgb_fit %>%
  extract_fit_parsnip() %>%
  vip::vip(geom = "point", num_features = 20)

ggsave(plot = vip_plot,
       filename = "Visualizations/vip_plot.png",
       width = 7,
       height = 5)

xgb_pred <-
  predict(xgb_fit, pitch_test, type = "prob") %>%
  bind_cols(pitch_test)

xgb_pred %>%
  roc_auc(truth = IsStrike, .pred_0) # 0.983

pred_heatmap <- xgb_pred %>%
  # filter(.pred_1 > 0.45 & .pred_1 < 0.55) %>%
  rename(strike_prob = .pred_1) %>%
  ggplot(aes(x = plate_x, y = plate_z, color = strike_prob)) +
  geom_raster(aes(fill = strike_prob), interpolate = TRUE) +
  scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF","#FF0000FF")) +
  geom_segment(x = 0.75, y = mean(xgb_pred$sz_bot), xend = 0.75, yend = mean(xgb_pred$sz_top), color = "black") +
  geom_segment(x = -0.75, y = mean(xgb_pred$sz_bot), xend = -0.75, yend = mean(xgb_pred$sz_top), color = "black") +
  geom_segment(x = 0.75, y = mean(xgb_pred$sz_bot), xend = -0.75, yend = mean(xgb_pred$sz_bot), color = "black") +
  geom_segment(x = 0.75, y = mean(xgb_pred$sz_top), xend = -0.75, yend = mean(xgb_pred$sz_top), color = "black") +
  coord_fixed()

ggsave(plot = pred_heatmap,
       filename = "Visualizations/prediction_heatmap.png",
       width = 5,
       height = 5)

last_fit(
  final_xgb,
  pitch_split,
  metrics = perf_metrics
) %>%
  collect_metrics()

# ppv:      0.950
# npv:      0.898
# AUC:      0.983
# logloss:  0.159

saveRDS(xgb_fit, "Modeling/strike_prob_xgb_tuned.Rds")