library(baseballstarR)
library(tidyverse)
library(DT)
library(trelliscopejs)
library(reactable)
library(crosstalk)
library(reactablefmtr)
library(htmltools)
source(here::here("R", "functions.R"))

# ----- INITIAL IMPORT: STATIC after initial run DO NOT REFRESH FROM HERE ----
# player list
players <- starrbaseball::mlb_playerids

# player_list <- get_cbs_mlb_players()

# ----- RUN FROM HERE ----
# get current draft results
# draft_results <- baseballstarR:::cbs_draft_results(include_bidding_history = FALSE)


# cbs rosters
# cbs_rosters <- baseballstarR:::cbs_rosters(league_name = "sammyfbl")


# player table with cbs info
player_tbl <- get_cbs_mlb_players() |> 
  mutate(age = lubridate::as.period(lubridate::interval(start = birthdate, end = lubridate::today()))$year)

eno_sp_ranks <- readr::read_csv(here::here("data-raw", "2024_eno-sarris-pitching-ranks.csv")) |> 
  mutate(mlb_id = as.character(mlb_id))

# ---- joins ----

pitching_join <- left_join(eno_sp_ranks, 
                           player_tbl |> select(mlb_id, name, cbs_team_name, age, salary, contract_year, fangraphs_stats_url, savant_stats_url, headshot_url))

# ---- table mutates ----


# Fully Searchable Table
pitching <- pitching_join %>%
  mutate(
    cbs_team_name = ifelse(is.na(cbs_team_name), "Free Agent", cbs_team_name),
    cbs_team_name = as.factor(cbs_team_name),
    headshot_url = glue::glue("https://img.mlbstatic.com/mlb-photos/image/upload/w_100,q_100/v1/people/{mlb_id}/headshot/silo/current"))
  



# works  paste0('"<a  target=_blank href=', urlPlayerStatsFG, '>', urlPlayerStatsFG,'</a>' ))

# "<a  target=_blank href=https://www.fangraphs.com/statss.aspx?playerid=13125>https://www.fangraphs.com/statss.aspx?playerid=13125</a>"

# ---- All Pitchers ----
pitching_tbl <- pitching %>%
  select(eno_rank, headshot_url, name, age, cbs_team_name, stuff_plus, location_plus, pitching_plus, dollars, injury_risk, salary, fangraphs_stats_url) 
# pitching_dt <- pitching_tbl %>%
# mutate(
#   urlPlayerStatsFG = glue::glue('<a target=_blank href={urlPlayerStatsFG}>{namePlayerMLB} Fangraphs</a>'),
#   headshot = glue::glue('<img src="{headshotURL}" height="40">')) %>%
#   relocate(headshot, .after = namePlayerMLB)
# # ---- Table
#
# pitching_dt %>%
#   datatable(style = "auto",
#             filter = 'top',
#             extensions = 'Buttons',
#             options = list(
#               dom = 'Bfrtip',
#               buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
#               pageLength = 100,
#               initComplete = JS(
#                 "function(settings, json) {",
#                 "$('body').css({'font-family': 'Roboto Condensed'});",
#                 "}"
#               )
#             ), escape = FALSE
#   )

htmltools::tags$link(href = "https://fonts.googleapis.com/css?family=Chivo:400,600,700&display=swap", rel = "stylesheet")

### create shared dataset for crosstalk
crosstalk_data <- SharedData$new(pitching_tbl)


### crosstalk team filter
team_filter <- filter_select(
  id = "team",
  label = "CBS TEAM",
  sharedData = crosstalk_data,
  group = ~ cbs_team_name
)

### crosstalk stuff filter
stuff_filter <- filter_slider(
  id = "stuff_plus",
  label = "STUFF+",
  sharedData = crosstalk_data,
  column = ~ stuff_plus
)

### crosstalk location filter
location_filter <- filter_slider(
  id = "location_plus",
  label = "LOCATION+",
  sharedData = crosstalk_data,
  column = ~ location_plus
)

### crosstalk pitching filter
pitching_filter <- filter_slider(
  id = "pitching_plus",
  label = "PITCHING+",
  sharedData = crosstalk_data,
  column = ~ pitching_plus
)


pitching_tbl_react<- 
  reactable(crosstalk_data,
    theme = fivethirtyeight(),
    style = list(fontFamily = "Roboto Condensed", fontSize = "14px"),
    defaultSorted = "eno_rank",
    defaultColDef = colDef(
      sortNALast = TRUE,
      minWidth = 10
    ),
    filterable = FALSE,
    defaultPageSize = 25,
    columns = list(
      rank = colDef(
        name = "#",
        width = 40
      ),
      headshot_url = colDef(
        name = "",
        cell = embed_img(height = 50, width = 50),
        width = 50
      ),
      name = colDef(
        cell = function(value, index) {
          # Render as a link
          url <- pitching_tbl$fangraphs_stats_url[index]
          htmltools::tags$a(href = url, target = "_blank", as.character(value))
        },
        name = "Name",
        defaultSortOrder = "desc"
      ),
      age = colDef(
        name = "Age",
        defaultSortOrder = "asc",
        align = "left",
        width = 50
      ),
      cbs_team_name = colDef(
        name = "CBS Team",
        defaultSortOrder = "desc",
        width = 180
      ),
      stuff_plus = colDef(
        name = "STUFF+",
        defaultSortOrder = "desc",
        align = "center"
      ),
      location_plus = colDef(
        name = "LOCATION+",
        defaultSortOrder = "desc",
        align = "center"
      ),
      pitching_plus = colDef(
        name = "PITCHING+",
        defaultSortOrder = "desc",
        align = "center"
      ),
      injury_risk = colDef(
        name = "PROJECTED IL",
        defaultSortOrder = "desc",
        align = "center"
      ),
      dollars = colDef(
        name = "$",
        defaultSortOrder = "desc",
        align = "center"
      ),
      fangraphs_stats_url = colDef(show = FALSE),
      salary = colDef(
        name = "CBS $",
        defaultSortOrder = "desc",
        align = "center"
      )
    ),
    highlight = TRUE
  )

bscols(
  widths = c(2,10),
  list(team_filter,
       stuff_filter,
       location_filter,
       pitching_filter),
pitching_tbl_react)



# ---- Available Pitchers ----



# guys who have great stuff metrics
great_stuff <- pitching %>%
  filter(stuff > 100 & location > 100 & pitching > 100 & nameTeamCBS == "Free Agent")


# Trelliscope
pitching_dashboard <- pitching %>%
  mutate(panel = img_panel(urlPlayerHeadshot)) %>%
  unique()


# Big Monitor
pitching_dashboard %>%
  mutate(panel = img_panel(urlPlayerHeadshot)) %>%
  trelliscope(
    name = "pitching", nrow = 3, ncol = 9,
    state = list(labels = c(
      "rank", "namePlayerMLB", "age", "nameTeamCBS",
      "stuff", "location", "pitching"
    ))
  )
