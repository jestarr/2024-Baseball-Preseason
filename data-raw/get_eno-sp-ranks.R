# Get Eno Starting Pitcher Rankings
library(starrbaseball)
# first get sheet names to know what to use
get_eno_sp_rankings(sheet_names = TRUE)

eno <- get_eno_sp_rankings(sheet = "WorkingRanksMarc28", sheet_names = FALSE) |> 
  dplyr::filter(!is.na(player_name))

eno_stuff <- get_eno_sp_rankings(sheet = "NewStuff+ Pitcher", sheet_names = FALSE) |> 
  dplyr::select(mlb_id = MLBAMID,
                player_name) |> 
  dplyr::mutate(mlb_id = as.character(mlb_id))

# join mlb id from stuff sheet
eno_ranks <- eno |> 
  dplyr::mutate(player_name = stringi::stri_trans_general(player_name, "Latin-ASCII")) |> 
  dplyr::left_join(eno_stuff) |> 
  dplyr::relocate(mlb_id, .after = player_name)

mlb_ids <- mlb_playerids |> 
  dplyr::filter(season >= 2022) |> 
  dplyr::select(player_name = name, mlb_id) |> 
  dplyr::mutate(player_name = stringi::stri_trans_general(player_name, "Latin-ASCII"))

# find duplicate player names
mlb_dupes <- mlb_ids |> 
  dplyr::count(player_name, sort = TRUE) |> 
  dplyr::filter(n > 1) |> 
  dplyr::pull(player_name)

# remove names that are duplicates
mlb_ids <- mlb_ids |> 
  dplyr::filter(!player_name %in% mlb_dupes)

eno_ranks <- eno_ranks |> 
  nflreadr::join_coalesce(mlb_ids, by = "player_name") |> 
  dplyr::arrange(eno_rank) |> 
  dplyr::filter(!is.na(eno_rank))

readr::write_excel_csv(eno_ranks, "data-raw/2024_eno-sarris-pitching-ranks.csv")
