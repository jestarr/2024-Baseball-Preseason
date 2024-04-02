library(starrbaseball)
library(dplyr)
library(crosstalk)
library(reactable)
source(here::here("R", "functions.R"))

# inspo: https://kcuilla.github.io/reactablefmtr/articles/nba_player_ratings.html


# projection_source = c("thebatx", "steamer")
stats_type = "bat"
bat_split = 60
teams = 12
budget = 270
season_type = "pre"
# 
# 
# 
# 



generate_bat_auction_crosstalk <- function(projection_source = c("steamer", "thebatx"),
                                           stats_type = "bat",
                                           bat_split = 60,
                                           teams = 12,
                                           budget = 270,
                                           season_type = "pre") {
  # ---- CBS Player Table ----
  player_tbl <- get_cbs_mlb_players() |>
    mutate(age = lubridate::as.period(lubridate::interval(start = birthdate, end = lubridate::today()))$year)

  # ---- Fangraphs Projections & Auction Calc Values
  bat_auction_calc <- starrbaseball::fg_auction_calculator(projection_source = projection_source,
                                                           stats_type = stats_type,
                                                           bat_split = bat_split,
                                                           teams = teams,
                                                           budget = budget) |> 
    mutate(across(starts_with("m"), ~round(.,1)),
           dollars = round(dollars, 1)
           )

  bat_proj <- fg_projections(projection_source = projection_source,
                             stats_type = stats_type,
                             season_type = season_type )
  
  bat_proj <- bat_proj |> 
    select(projection_source, player_name, fangraphs_id, PA, R, HR, RBI, SB, OBP) |> 
    mutate(across(c(PA, R, HR, RBI, SB), ~ round(., 0)),
           OBP = round(OBP, 3))
  
  
  
  # ---- joins ----
  batting <- bat_proj |> left_join(
    bat_auction_calc |> select(projection_source, fangraphs_id = playerid, dollars)) |> 
      left_join(
    player_tbl |> select(mlb_id, fangraphs_id, name, cbs_team_name, cbs_position, eligible_position, age, fangraphs_stats_url, savant_stats_url, headshot_url, cbs_logo_url)
  )
  
  batting_tbl <- batting %>%
    mutate(
      cbs_team_name = ifelse(is.na(cbs_team_name), "Free Agent", cbs_team_name),
      cbs_team_name = as.factor(cbs_team_name),
      headshot_url = glue::glue("https://img.mlbstatic.com/mlb-photos/image/upload/w_100,q_100/v1/people/{mlb_id}/headshot/silo/current")
    ) |>
    select(headshot_url, name, age, cbs_team_name, cbs_position, eligible_position, age, PA, R, HR, RBI, SB, OBP,  dollars, fangraphs_stats_url, cbs_logo_url) 
  
  
  ### create shared dataset for crosstalk
  crosstalk_data <- SharedData$new(batting_tbl)
  
  
  ### crosstalk team filter
  team_filter <- filter_select(
    id = "team",
    label = "CBS TEAM",
    sharedData = crosstalk_data,
    group = ~cbs_team_name
  )
  
  ### crosstalk position filter
  position_filter <- filter_select(
    id = "cbs_position",
    label = "Position",
    sharedData = crosstalk_data,
    group = ~cbs_position
  )
  
  
  batting_tbl_react <-
    reactable(crosstalk_data,
              theme = reactablefmtr::fivethirtyeight(font_size = 12),
              compact = TRUE,
              style = list(fontFamily = "Roboto Condensed"),
              searchable = TRUE,
              language = reactableLang(
                searchPlaceholder = "SEARCH FOR A PLAYER..."),
              defaultPageSize = 100,
              defaultSorted = "dollars",
              defaultColDef = colDef(
                sortNALast = TRUE
              ),
              filterable = FALSE,
              columns = list(
                headshot_url = colDef(
                  maxWidth = 50,
                  align =  "center",
                  name = "",
                  cell = reactablefmtr::embed_img(height = 40, width = 40),
                ),
                name = colDef(
                  cell = function(value, index) {
                    # Render as a link
                    url <- batting_tbl$fangraphs_stats_url[index]
                    htmltools::tags$a(href = url, target = "_blank", as.character(value))
                  },
                  name = "Name",
                  width = 150
                ),
                age = colDef(
                  name = "Age",
                  defaultSortOrder = "asc",
                  align = "left",
                  width = 50
                ),
                cbs_team_name = colDef(
                  name = "CBS Team",
                  width = 180
                ),
                cbs_position = colDef(
                  name = "CBS Pos.",
                  align = "center",
                  width = 80
                ),
                eligible_position = colDef(
                  name = "Eligible Pos.",
                  width = 70
                ),
                R = colDef(
                  name = "R",
                  width = 60
                ),
                HR = colDef(
                  name = "HR",
                  width = 60
                ),
                RBI = colDef(
                  name = "RBI",
                  width = 60
                ),
                SB = colDef(
                  name = "SB",
                  width = 60
                ),
                OBP = colDef(
                  name = "OBP",
                  width = 60
                ),
                fangraphs_stats_url = colDef(show = FALSE),
                cbs_logo_url = colDef(show = FALSE),
                dollars = colDef(
                  name = "$",
                  defaultSortOrder = "desc",
                  align = "center",
                  width = 60
                )
              ),
              highlight = TRUE)
  
  bscols(
    widths = c(2, 10),
    list(
      team_filter,
      position_filter
    ),
    batting_tbl_react
  )
}

# Generate Auction
generate_bat_auction_crosstalk(projection_source = "thebatx",
                              bat_split = 65)
