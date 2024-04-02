library(baseballstarR)
library(tidyverse)
library(DT)
library(trelliscopejs)
source(here::here("R", "functions.R"))

players <- get_cbs_mlb_players(league_id = "sammyfbl")


players_mod <-  players %>% 
  filter(season >= 2018)
  
# use below if Keepers aren't in
# players_mod <- players %>% 
#   filter(yearSeason >= 2020) %>% 
#   mutate(salaryPlayer = case_when(
#     is.na(nameTeamCBS) ~ 0,
#     contractPlayer == 3 ~ 0,
#     TRUE ~ salaryPlayer + 5
#   ),
#   nameTeamCBS = ifelse(contractPlayer == 3, "Free Agent", nameTeamCBS)) %>% 
#   mutate(
#   nameTeamCBS = ifelse(contractPlayer == 0, "Free Agent", nameTeamCBS),
#   contractPlayer = case_when(
#     contractPlayer == 3 ~ 0,
#     is.na(nameTeamCBS) ~ 1,
#     TRUE ~ contractPlayer + 1
#   ))
           
          
# players table with images
images <- players_mod %>% 
  mutate(headshot = glue::glue('<img src ="{headshot_url}" height="52"></img>')) %>% 
  select(name, headshot)

eno_ranks <- read_csv(here::here("data-raw", "2024_eno-sarris-pitching-ranks.csv")) |> 
  mutate(mlb_id = as.character(mlb_id))


pitching <- left_join(eno_ranks, players_mod %>% select(mlb_id, name, cbs_team_name, contract_year, salary, birthdate, fangraphs_stats_url, savant_stats_url, headshot_url)) |> 
  mutate(age = lubridate::as.period(lubridate::interval(start = birthdate, end = lubridate::today()))$year)

#Fully Searchable Table
pitching <- pitching %>% 
  select(mlb_id, eno_rank, name, age, injury_risk,cbs_team_name, everything()) |> 
select(-player_name)

datatable <- pitching %>% 
  select(-headshot_url)


datatable(datatable, class = 'cell-border stripe', filter = 'top', 
          options = list(pageLength = 100, autoWidth = TRUE,
                         columnDefs = list(list(className = 'dt-center', targets = 0:6))))

# Trelliscope
pitching_dashboard <- pitching %>% 
  mutate(panel = img_panel(headshot_url)) %>% 
  unique() |> 
  select(eno_rank,
         name,
         age,
         injury_risk,
         cbs_team_name,
         stuff_plus,
         location_plus,
         pitching_plus,
         panel,
         headshot_url)


# Big Monitor
pitching_dashboard %>% 
  mutate(panel = img_panel(headshot_url)) %>% 
  trelliscope(name = "pitching", nrow = 3, ncol = 9,
              state = list(labels = c( "eno_rank","name", "age", "injury_risk",
                                       "cbs_team_name",
                                       "stuff_plus", "location_plus", "pitching_plus"))
              )
