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
xgb_fit <- readRDS("Modeling//strike_prob_xgb_tuned.Rds")
pitch_data <- read_csv("Data//pitch_data.csv")

factor_vars <- c("pitch_type", "p_throws", "stand", "inning_topbot", "runners_on", "risp", "pitch_group", "IsStrike")
pitch_data[factor_vars] <- lapply(pitch_data[factor_vars], factor)

pitch_pred <- xgb_fit %>%
  predict(pitch_data, type = "prob") %>%
  bind_cols(pitch_data)

#### VISUALIZATIONS ####

## Fit strike predictions to smooth grid
# function takes 2-3 minutes to run
plot_zone <- function(data, pitcher_hand = "ALL", batter_stance = "ALL") {
  if (pitcher_hand %in% c("R","L")) {
    data <- data %>% filter(p_throws == pitcher_hand)
  }
  if (batter_stance %in% c("R","L")) {
    data <- data %>% filter(stand == pitcher_hand)
  }
  
  ## Grid of strike zone
  pitch_grid <- expand.grid(plate_x = seq(-2, 2, 0.01), 
                            plate_z = seq(0, 5, 0.01))
  
  ## Sample 10,000 pitches for smoothing (sampling for time)
  loess_fit_grid <- loess(.pred_1 ~ plate_x + plate_z, data = data[sample(1:nrow(data), 10000),], control = loess.control(surface = "direct"))
  
  loess_pred_grid <-
    pitch_grid %>%
    mutate(pred = as.numeric(predict(loess_fit_grid, pitch_grid)))
  
  plot <- loess_pred_grid %>%
    ggplot(aes(x = plate_x, y = plate_z)) +
    stat_contour(aes(z = pred), color = "black", size = 1, breaks = 0.5) + 
    # strike zone
    geom_segment(x = 0.708, y = mean(data$sz_bot), xend = 0.708, yend = mean(data$sz_top), color = "black") +
    geom_segment(x = -0.708, y = mean(data$sz_bot), xend = -0.708, yend = mean(data$sz_top), color = "black") +
    geom_segment(x = 0.708, y = mean(data$sz_bot), xend = -0.708, yend = mean(data$sz_bot), color = "black") +
    geom_segment(x = 0.708, y = mean(data$sz_top), xend = -0.708, yend = mean(data$sz_top), color = "black") +
    # strike zone + one ball
    geom_segment(x = 0.708 + 0.125, y = mean(data$sz_bot) - 0.125, xend = 0.708 + 0.125, yend = mean(data$sz_top) + 0.125, color = "black", linetype = "dashed", size = 0.125) +
    geom_segment(x = -0.708 - 0.125, y = mean(data$sz_bot) - 0.125, xend = -0.708 - 0.125, yend = mean(data$sz_top) + 0.125, color = "black", linetype = "dashed", size = 0.125) +
    geom_segment(x = 0.708 + 0.125, y = mean(data$sz_bot) - 0.125, xend = -0.708 - 0.125, yend = mean(data$sz_bot) - 0.125, color = "black", linetype = "dashed", size = 0.125) +
    geom_segment(x = 0.708 + 0.125, y = mean(data$sz_top) + 0.125, xend = -0.708 - 0.125, yend = mean(data$sz_top) + 0.125, color = "black", linetype = "dashed", size = 0.125) +
    # home plate
    geom_segment(x = 0.708, y = 0, xend = -0.708, yend = 0) +
    geom_segment(x = -0.708, y = 0, xend = -0.8, yend = -0.25) +
    geom_segment(x = 0.708, y = 0, xend = 0.8, yend = -0.25) +
    geom_segment(x = -0.8, y = -0.25, xend = 0, yend = -0.5) + 
    geom_segment(x = 0.8, y = -0.25, xend = 0, yend = -0.5) +
    scale_x_continuous("Horizontal location (ft.)", limits = c(-2.0, 2.0)) + 
    scale_y_continuous("Vertical location (ft.)", limits = c(-1.0, 5.0)) +
    {if(pitcher_hand == "ALL" & batter_stance == "ALL") labs(title = "All Pitchers vs. All Batters", 
                                                             caption = "*Catcher's perspective")} + 
                                                             {if(pitcher_hand %in% c("R","L") & batter_stance %in% c("R","L")) labs(title = paste0(pitcher_hand, "HP vs. ", batter_stance, "HB"), 
                                                                                                                                    caption = "*Catcher's perspective")} +
                                                                                                                                    {if(pitcher_hand %in% c("R","L") & batter_stance == "ALL") labs(title = paste0(pitcher_hand, "HP vs. All Batters"), 
                                                                                                                                                                                                    caption = "*Catcher's perspective")} +
                                                                                                                                                                                                    {if(pitcher_hand == "ALL" & batter_stance %in% c("R","L")) labs(title = paste0("All Pitchers vs. ", batter_stance, "HB"), 
                                                                                                                                                                                                                                                                    caption = "*Catcher's perspective")} +
    coord_fixed()
  
  return(plot)
}

## ALL PITCHES
plot_zone(pitch_pred)

## RvR, RvL, LvR, LvL
rvr_plot <- plot_zone(data = pitch_pred, pitcher_hand = "R", batter_stance = "R")
rvl_plot <- plot_zone(data = pitch_pred, pitcher_hand = "R", batter_stance = "L")
lvr_plot <- plot_zone(data = pitch_pred, pitcher_hand = "L", batter_stance = "R")
lvl_plot <- plot_zone(data = pitch_pred, pitcher_hand = "L", batter_stance = "L")

# grid of platoon strikezones
cowplot::plot_grid(rvr_plot, rvl_plot,
                   lvr_plot, lvl_plot,
                   nrow = 2)
