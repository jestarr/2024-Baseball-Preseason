# ---- Generate in-season projected standings 
library(starrbaseball)
library(cbsapi)
library(tidyverse)
library(gt)

# Get Player list for ID Matching
mlb_players <- mlb_playerids

# Get CBS Rosters
cbs_rosters <- cbs_rosters()

# filter to only active players
cbs_rosters_active <- cbs_rosters |> 
  filter(roster_status == "A") |> 
  mutate(roster_group = ifelse(eligible_position == "P", "Pitchers", "Batters"))

cbs_rosters_active <- cbs_rosters_active |> 
  select(cbs_team_name, cbs_logo_url, cbs_id, name, position, eligible_position, roster_group)

# join other ids 
cbs_rosters_active <- cbs_rosters_active |> 
  left_join(mlb_players |> select(mlb_id, cbs_id, fangraphs_id))

# ---- Get Fangraphs Projections ----
bat_proj <- starrbaseball::fg_projections(projection_source = c("atc", "thebat", "thebatx", "fangraphsdc", "steamer", "zips", "zipsdc"),
                                                      stats_type = "bat",
                                                      season_type = "pre") 
pit_proj <- starrbaseball::fg_projections(projection_source = c("atc", "thebat", "fangraphsdc", "steamer", "zips", "zipsdc"),
                                                      stats_type = "pit",
                                                      season_type = "pre") 


# ---- Batters ----

cbs_batters <- cbs_rosters_active |> 
  filter(roster_group == "Batters") |> 
  left_join(bat_proj |> select(projection_source, fangraphs_id, H, BB, HBP, AB, SF, PA, R, HR, RBI, SB))


cbs_batters_team_totals <- cbs_batters |> 
  group_by(projection_source, cbs_team_name) |> 
  summarise(across(c("H", "BB", "HBP", "AB", "SF", "PA", "R", "HR", "RBI", "SB"), ~ sum(.x))) |> 
  mutate(OBP = (H + BB + HBP) / (AB + BB + HBP + SF)
  ) |> 
  select(projection_source, cbs_team_name, PA, AB, R, HR, RBI, SB, OBP) |> 
  ungroup() |> 
  mutate(across(c(PA, AB, R, HR, RBI, SB), round),
         OBP = round(OBP, 3)) |> 
  group_by(projection_source) |> 
  mutate(across(c(R, HR, RBI, SB, OBP), rank, .names = "{.col}_points"),
         bat_points = R_points + HR_points + RBI_points + SB_points + OBP_points,
         bat_rank = rank(-bat_points))

cbs_batters_team_totals |> gt()


# ---- Pitchers ----
cbs_pitchers <- cbs_rosters_active |> 
  filter(roster_group == "Pitchers") |> 
  left_join(pit_proj |> select(projection_source, fangraphs_id, IP, H, BB, W, ER, SO, SV, HLD)) |> 
  mutate(SOLDS = SV + HLD)


cbs_pitchers_team_totals <- cbs_pitchers |> 
  group_by(projection_source, cbs_team_name) |> 
  summarise(across(c(IP, H, BB, W, ER, SO, SV, HLD), ~ sum(.x)),
            num_relievers = sum(position == "RP")) |> 
  mutate(ERA = (ER * 9) / IP,
         WHIP = (BB + H) / IP,
         SOLDS = SV + HLD
  ) |> 
  select(projection_source, cbs_team_name, IP, W, SO, SOLDS, ERA, WHIP) |> 
  ungroup() |> 
  mutate(across(c(W, SO, SOLDS), round),
         across(c(ERA, WHIP),  ~ round(., 2))) |> 
  group_by(projection_source) |> 
  mutate(across(c(W, SO, SOLDS), rank, .names = "{.col}_points"),
         across(c(ERA, WHIP), ~rank(-.x), .names = "{.col}_points"),
         pitch_points = W_points + SO_points + SOLDS_points + ERA_points + WHIP_points,
         pitch_rank = rank(-pitch_points))

# ---- Team Ranks ----
team_ranks <- left_join(cbs_batters_team_totals, cbs_pitchers_team_totals) |> 
  select(projection_source, cbs_team_name, bat_points, pitch_points) |> 
  mutate(total_points = bat_points + pitch_points,
         bat_rank = rank(-bat_points),
         pitch_rank = rank(-pitch_points),
         rank = rank(-total_points)) |> 
  arrange(cbs_team_name)

# Hitter Ranks
team_ranks |> 
  select(projection_source, cbs_team_name, bat_rank) |> 
  pivot_wider(names_from = projection_source,
              values_from = bat_rank
  ) |> 
  rowwise() |> 
  mutate(avg_rank = mean(c(ATC, `Depth Charts`, Steamer, `THE BAT`, `THE BAT X`,ZiPS, `ZiPS DC`)),
         avg_rank = round(avg_rank, 1))

# Pitcher Ranks
team_ranks |> 
  # The Bat X doesn't project pitching
  filter(projection_source != "THE BAT X") |> 
  select(projection_source, cbs_team_name, pitch_rank) |> 
  pivot_wider(names_from = projection_source,
              values_from = pitch_rank
  ) |> 
  rowwise() |> 
  mutate(avg_rank = mean(c(ATC, `Depth Charts`, Steamer, `THE BAT`, ZiPS, `ZiPS DC`)),
         avg_rank = round(avg_rank, 1)) |> 
  gt()



team_ranks_wide <- team_ranks |> 
  select(projection_source, cbs_team_name, rank) |> 
  pivot_wider(names_from = projection_source,
              values_from = rank
  ) |> 
  rowwise() |> 
  mutate(avg_rank = mean(c(ATC, `Depth Charts`, Steamer, `THE BAT`, ZiPS, `ZiPS DC`)),
         avg_rank = round(avg_rank, 1))

team_ranks_wide |> 
  gt()


