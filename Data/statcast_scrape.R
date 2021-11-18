# install.packages("devtools")
# install.packages("remotes")
# devtools::install_github("BillPetti/baseballr")

# This script was written based off of code and articles from Bill Petti using his baseballr package.

library(baseballr)
library(tidyverse)
library(DBI)
library(RPostgreSQL)
library(RPostgres)
library(data.table)

annual_statcast_query <- function(season) {
  
  dates <- seq.Date(as.Date(paste0(season, '-03-01')),
                    as.Date(paste0(season, '-12-01')), by = 'week')
  
  date_grid <- tibble(start_date = dates, 
                      end_date = dates + 6)
  
  safe_savant <- safely(scrape_statcast_savant)
  
  payload <- map(.x = seq_along(date_grid$start_date), 
                 ~{message(paste0('\nScraping week of ', date_grid$start_date[.x], '...\n'))
                   
                   payload <- safe_savant(start_date = date_grid$start_date[.x], 
                                          end_date = date_grid$end_date[.x], type = 'pitcher')
                   
                   return(payload)
                 })
  
  payload_df <- map(payload, 'result')
  
  number_rows <- map_df(.x = seq_along(payload_df), 
                        ~{number_rows <- tibble(week = .x, 
                                                number_rows = length(payload_df[[.x]]$game_date))}) %>%
    filter(number_rows > 0) %>%
    pull(week)
  
  payload_df_reduced <- payload_df[number_rows]
  
  combined <- payload_df_reduced %>%
    bind_rows()
  
  return(combined)
  
}


format_append_statcast <- function(df) {
  
  # function for appending new variables to the data set
  
  additional_info <- function(df) {
    
    # apply additional coding for custom variables
    
    df$hit_type <- with(df, ifelse(events == "single", 1,
                                   ifelse(events == "double", 2,
                                          ifelse(events == "triple", 3, 
                                                 ifelse(events == "home_run", 4, NA)))))
    
    df$hit <- with(df, ifelse(events == "single", 1,
                              ifelse(events == "double", 1,
                                     ifelse(events == "triple", 1, 
                                            ifelse(events == "home_run", 1, NA)))))
    
    df$B1 <- with(df, ifelse(events == "single", 1, NA))
    df$B2 <- with(df, ifelse(events == "double", 1, NA))
    df$B3 <- with(df, ifelse(events == "triple", 1, NA))
    df$HR <- with(df, ifelse(events == "home_run", 1, NA))
    df$SO <- with(df, ifelse(events %like% "strikeout", 1, NA))
    df$UBB <- with(df, ifelse(events == "walk", 1, NA))
    df$IBB <- with(df, ifelse(events == "intent_walk", 1, NA))
    df$HBP <- with(df, ifelse(events == "hit_by_pitch", 1, NA))
    df$SF <- with(df, ifelse(events %like% "sac_fly", 1, NA))
    df$SH <- with(df, ifelse(events %like% "sac_bunt", 1, NA))
    
    df$AB <- with(df, ifelse(events == "single", 1,
                             ifelse(events == "double", 1,
                                    ifelse(events == "triple", 1, 
                                           ifelse(events == "home_run", 1,
                                                  ifelse(events %like% "out", 1,
                                                         ifelse(events %like% "play", 1,
                                                                ifelse(events %like% "error", 1, NA))))))))
    
    df$PA <- with(df, ifelse(events == "single", 1,
                             ifelse(events == "double", 1,
                                    ifelse(events == "triple", 1, 
                                           ifelse(events == "home_run", 1,
                                                  ifelse(events %like% "out", 1,
                                                         ifelse(events %like% "play", 1,
                                                                ifelse(events %like% "error", 1,
                                                                       ifelse(events %like% "walk", 1,
                                                                              ifelse(events == "hit_by_pitch", 1,
                                                                                     ifelse(events %like% "sac", 1,
                                                                                            ifelse(events == "interf_def", 1, NA))))))))))))
    
    df$fielding_team <- with(df, ifelse(inning_topbot == "Bot", away_team, home_team))
    
    df$batting_team <- with(df, ifelse(inning_topbot == "Bot", home_team, away_team))
    
    df <- df %>%
      mutate(barrel = ifelse(launch_angle <= 50 & launch_speed >= 98 & launch_speed * 1.5 - launch_angle >= 117 & launch_speed + launch_angle >= 124, 1, 0))
    
    df <- df %>%
      mutate(spray_angle = round(
        (atan(
          (hc_x-125.42)/(198.27-hc_y)
        )*180/pi*.75)
        ,1)
      )
    
    df <- df %>%
      filter(!is.na(game_year))
    
    return(df)
  }
  
  df <- df %>%
    additional_info()
  
  df$game_date <- as.character(df$game_date)
  
  df <- df %>%
    arrange(game_date)
  
  df <- df %>%
    filter(!is.na(game_date))
  
  df <- df %>%
    ungroup()
  
  df <- df %>%
    select(setdiff(names(.), c("error")))
  
  cols_to_transform <- c("fielder_2", "pitcher_1", "fielder_2_1", "fielder_3",
                         "fielder_4", "fielder_5", "fielder_6", "fielder_7",
                         "fielder_8", "fielder_9")
  
  df <- df %>%
    mutate_at(.vars = cols_to_transform, as.numeric) %>%
    mutate_at(.vars = cols_to_transform, function(x) {
      ifelse(is.na(x), 999999999, x)
    })
  
  data_base_column_types <- read_csv("https://app.box.com/shared/static/q326nuker938n2nduy81au67s2pf9a3j.csv")
  
  character_columns <- data_base_column_types %>%
    filter(class == "character") %>%
    pull(variable)
  
  numeric_columns <- data_base_column_types %>%
    filter(class == "numeric") %>%
    pull(variable)
  
  integer_columns <- data_base_column_types %>%
    filter(class == "integer") %>%
    pull(variable)
  
  df <- df %>%
    mutate_if(names(df) %in% character_columns, as.character) %>%
    mutate_if(names(df) %in% numeric_columns, as.numeric) %>%
    mutate_if(names(df) %in% integer_columns, as.integer)
  
  df <- df %>%
    mutate(player_name = stringi::stri_trans_general(str = player_name,
                                            id = "Latin-ASCII"))
  
  return(df)
}

delete_and_upload <- function(df, 
                              year, 
                              dbname) {
  
  statcast_db <- dbConnect(RPostgres::Postgres(), dbname = dbname)
  
  query <- paste0('DELETE from statcast where game_year = ', year)
  
  dbGetQuery(statcast_db, query)
  
  dbWriteTable(statcast_db, "statcast", df, append = TRUE)
  
  dbDisconnect(statcast_db)
  rm(statcast_db)
}

# # create table and upload first year
# 
# payload_statcast <- annual_statcast_query(2015)
# 
# df <- format_append_statcast(df = payload_statcast)
# 
# # connect to your database
# # here I am using my personal package that has a wrapper function for this
# 
# statcast_db <- dbConnect(RPostgres::Postgres(), dbname = "statcast")
# dbListTables(statcast_db)
# 
# dbWriteTable(statcast_db, "statcast", df, overwrite = TRUE)
# 
# tbl(statcast_db, 'statcast') %>%
#   filter(game_year == 2015) %>%
#   count()
# 
# dbDisconnect(statcast_db)
# 
# rm(df)
# gc()
# 
# map(.x = seq(2016, 2020, 1),
#     ~{payload_statcast <- annual_statcast_query(season = .x)
# 
#     message(paste0('Formatting payload for ', .x, '...'))
# 
#     df <- format_append_statcast(df = payload_statcast)
# 
#     message(paste0('Deleting and uploading ', .x, ' data to database...'))
# 
#     delete_and_upload(df,
#                       year = .x,
#                       dbname = 'statcast')
# 
#     statcast_db <- dbConnect(RPostgres::Postgres(), dbname = "statcast")
# 
#     dbGetQuery(statcast_db, 'select game_year, count(game_year) from statcast group by game_year')
# 
#     dbDisconnect(statcast_db)
# 
#     message('Sleeping and collecting garbage...')
# 
#     Sys.sleep(5*60)
# 
#     gc()
# 
#     })
# 
# statcast_db <- dbConnect(RPostgres::Postgres(), dbname = "statcast")

##################################################################################
# update 2021 data
map(.x = seq(2021, 2021, 1),
    ~{payload_statcast <- annual_statcast_query(season = .x)
    
    message(paste0('Formatting payload for ', .x, '...'))
    
    df <- format_append_statcast(df = payload_statcast)
    
    message(paste0('Deleting and uploading ', .x, ' data to database...'))
    
    delete_and_upload(df,
                      year = .x,
                      dbname = 'statcast')
    
    statcast_db <- dbConnect(RPostgres::Postgres(), dbname = "statcast")
    
    dbGetQuery(statcast_db, 'select game_year, count(game_year) from statcast group by game_year')
    
    dbDisconnect(statcast_db)
    
    # message('Sleeping and collecting garbage...')
    # 
    # Sys.sleep(5*60)
    
    gc()
    
    })

statcast_db <- dbConnect(RPostgres::Postgres(), dbname = "statcast")

tbl(statcast_db, 'statcast') %>%
  group_by(game_year) %>%
  count() %>%
  collect()

