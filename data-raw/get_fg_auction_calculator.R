library(starrbaseball)

# players <- get_historical_player_tbl(league_name = "sammyfbl")

# ---- Sammy Player Values ----

# get auction calculator values
bat_values <- starrbaseball::fg_auction_calculator(projection_source = c("atc", "thebat", "thebatx", "fangraphsdc", "steamer","2023", "2022", "2021"),
                                                   stats_type = "bat",
                                                   bat_split = 60,
                                                   teams = 12,
                                                   budget = 270)
  
  
  
  
  auction_calc_tbl(projection_sources = c("atc", "fangraphsdc", "steamer", "thebat", "thebatx"),
                               teams = 12,
                               budget = 270,
                               stats_type = "bat",
                               bat_split = 60,
                               compact = TRUE)

bat_values_summary <- auction_calc_value_summary(projection_sources = c("atc", "fangraphsdc", "steamer", "thebat", "thebatx"),
                                                 teams = 12,
                                                 budget = 270,
                                                 stats_type = "bat",
                                                 bat_split = 60,
                                                 compact = TRUE)




# write rds file
saveRDS(bat_values, here::here("data", "2023_batter_auction_calc_values(sammyfbl).rds"))
saveRDS(bat_values_summary, here::here("data", "2023_batter_auction_calc_values_summary(sammyfbl).rds"))


pitch_values <- auction_calc_tbl(projection_sources = c("atc", "fangraphsdc", "steamer", "thebat", "thebatx"),
                                 teams = 12,
                                 budget = 270,
                                 stats_type = "pit",
                                 bat_split = 60,
                                 compact = TRUE)

pitch_values_summary <- auction_calc_value_summary(projection_sources = c("atc", "fangraphsdc", "steamer","thebat", "thebatx"),
                                                   teams = 12,
                                                   budget = 270,
                                                   stats_type = "pit",
                                                   bat_split = 60,
                                                   compact = TRUE)


# write rds file
saveRDS(pitch_values, here::here("data", "2023_auction_calc_values_pitching(sammyfbl).rds"))
saveRDS(pitch_values_summary, here::here("data", "2023_fg_auction_calc_values_summary_pitching(sammyfbl).rds"))
