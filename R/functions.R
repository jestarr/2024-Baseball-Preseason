library(tidyverse)
library(baseballstarR)

league_id = "sammyfbl"


get_cbs_mlb_players <- function(league_id = "sammyfbl") {
  # Get Player ID Map
  mlb_players <- starrbaseball::mlb_playerids 

  # Get CBS Players
  cbs_players <- cbsapi::cbs_player_search(league_id = league_id)
  cbs_teams <- cbsapi::cbs_teams(league_id = "sammyfbl")
  
  # Subset CBS Players to necessary columns
  cbs_players <- cbs_players |> 
    left_join(cbs_teams |> select(cbs_team_name, cbs_logo_url)) |> 
    dplyr::select(cbs_id,
                  cbs_player_name,
                  cbs_team_name,
                  cbs_position = position,
                  eligible_position,
                  salary,
                  contract_year,
                  cbs_logo_url) |> 
    dplyr::mutate(cbs_position = ifelse(cbs_position %in% c("LF", "CF", "RF"), "OF", cbs_position))
    
   player_tbl <- mlb_players %>% 
    dplyr::left_join(cbs_players, by = "cbs_id")
   
  return(player_tbl)
}
