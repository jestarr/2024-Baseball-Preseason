library(tidyverse)
library(starrbaseball)


season <- "2024"

# ---- batting projections ----
bat_proj <- starrbaseball::fg_projections(projection_source = c("atc", "thebat", "thebatx", "fangraphsdc", "steamer", "zips", "zipsdc"),
                                          stats_type = "bat",
                                          season_type = "pre") 


# single projection scrape and raw files
# starrbaseball::fg_projections(projection_source = c("atc", "thebat", "thebatx", "fangraphsdc", "steamer", "zips", "zipsdc"),
#                               stats_type = "bat",
#                               season_type = "pre") |> 
#   dplyr::group_by(projection_source) |> 
#   dplyr::group_walk(~ write.csv(.x, 
#                 here::here("data-raw", "projections", paste0("2024_", .y, "_projections_bat.csv"))
#                 )
#   )



# Practice Glue
bat_proj |>
  dplyr::group_by(projection_source) |>
  dplyr::group_walk(~ write.csv(
    .x,
    here::here("data-raw", "projections", glue::glue("{season}_{.y}_projections_bat.csv"))
  ))
  


# bat_proj <- bat_proj %>% 
#   left_join(players %>% select(idPlayerMLB, namePlayerMLB), by = c("id" = "idPlayerMLB")) %>% 
#   select(sourceProjection, id, idProjection, namePlayerMLB, everything())
# 
# 
# bat_proj %>% 
#   group_by(sourceProjection) %>% 
#   group_walk(~ write.csv(.x, here::here("data", "projections", paste0("2022_", .y$sourceProjection, "_projections_bat.csv"
#   )))
#   )

saveRDS(bat_proj, here::here("data", "projections",   paste0("2024_combined_fg_hitting_projections.rds")))

# ---- pitching projections ----
# write raw files for each projection set
pit_proj <- starrbaseball::fg_projections(projection_source = c("atc", "thebat", "fangraphsdc", "steamer", "zips", "zipsdc"),
                                          stats_type = "pit",
                                          season_type = "pre") 




# pit_proj <- pit_proj %>% 
#   left_join(players %>% select(idPlayerMLB, namePlayerMLB), by = c("id" = "idPlayerMLB")) %>% 
#   select(sourceProjection, id, idProjection, namePlayerMLB, everything())


pit_proj |>
  dplyr::group_by(projection_source) |>
  dplyr::group_walk(~ write.csv(
    .x,
    here::here("data-raw", "projections", glue::glue("{season}_{.y}_projections_pit.csv"))
  ))

saveRDS(pit_proj, here::here("data",  "projections", paste0("2024_combined_fg_pitching_projections.rds")))
