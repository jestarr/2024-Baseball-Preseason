library(starrbaseball)
library(dplyr)
library(crosstalk)
library(reactable)
library(reactablefmtr)
source(here::here("R", "functions.R"))

# inspo: https://kcuilla.github.io/reactablefmtr/articles/nba_player_ratings.html

generate_sp_auction_crosstalk <- function() {
  # ---- CBS Player Table ----
  player_tbl <- get_cbs_mlb_players() |>
    mutate(age = lubridate::as.period(lubridate::interval(start = birthdate, end = lubridate::today()))$year)
  # ---- Eno Sarris SP Ranks ----
  eno_sp_ranks <- readr::read_csv(here::here("data-raw", "2024_eno-sarris-pitching-ranks.csv")) |>
    mutate(mlb_id = as.character(mlb_id))

  # ---- Fangraphs Projections & Auction Calc Values
  pit_auction_calc <- read_rds(here::here("data", "2024_auction_calc_values_pitching(sammyfbl).rds"))
  pit_prior_ytd <- read_rds(here::here("data", "2021-2023YTD_auction_calc_values_pitching(sammyfbl).rds"))
  pit_auction_calc_summary <- read_rds(here::here("data", "2024_fg_auction_calc_values_summary_pitching(sammyfbl).rds"))

  pit_prior_ytd <- pit_prior_ytd |>
    select(projection_source, playerid, player_name, dollars) |>
    pivot_wider(
      names_from = projection_source,
      values_from = dollars
    ) |>
    janitor::clean_names()


  pit_auction_calc_summary <- pit_auction_calc_summary |>
    left_join(pit_prior_ytd)

  # ---- joins ----
  pitching <- left_join(
    eno_sp_ranks,
    player_tbl |> select(mlb_id, fangraphs_id, name, cbs_team_name, age, salary, contract_year, fangraphs_stats_url, savant_stats_url, headshot_url, cbs_logo_url) |>
      left_join(pit_auction_calc_summary |> select(fangraphs_id = playerid, everything()))
  )

  pitching_tbl <- pitching %>%
    mutate(
      cbs_team_name = ifelse(is.na(cbs_team_name), "Free Agent", cbs_team_name),
      cbs_team_name = as.factor(cbs_team_name),
      headshot_url = glue::glue("https://img.mlbstatic.com/mlb-photos/image/upload/w_100,q_100/v1/people/{mlb_id}/headshot/silo/current")
    ) |>
    select(eno_rank, headshot_url, name, age, cbs_team_name, stuff_plus, location_plus, pitching_plus, salary, starts_with("dollars_"), injury_risk, fangraphs_stats_url, cbs_logo_url)


  ### create shared dataset for crosstalk
  crosstalk_data <- SharedData$new(pitching_tbl)


  ### crosstalk team filter
  team_filter <- filter_select(
    id = "team",
    label = "CBS TEAM",
    sharedData = crosstalk_data,
    group = ~cbs_team_name
  )

  ### crosstalk stuff filter
  stuff_filter <- filter_slider(
    id = "stuff_plus",
    label = "STUFF+",
    sharedData = crosstalk_data,
    column = ~stuff_plus
  )

  ### crosstalk location filter
  location_filter <- filter_slider(
    id = "location_plus",
    label = "LOCATION+",
    sharedData = crosstalk_data,
    column = ~location_plus
  )

  ### crosstalk pitching filter
  pitching_filter <- filter_slider(
    id = "pitching_plus",
    label = "PITCHING+",
    sharedData = crosstalk_data,
    column = ~pitching_plus
  )


  pitching_tbl_react <-
    reactable(crosstalk_data,
      theme = reactablefmtr::fivethirtyeight(font_size = 12),
      compact = TRUE,
      style = list(fontFamily = "Roboto Condensed"),
      searchable = TRUE,
      language = reactableLang(
        searchPlaceholder = "SEARCH FOR A PLAYER..."),
      defaultPageSize = 100,
      defaultSorted = "eno_rank",
      defaultColDef = colDef(
        sortNALast = TRUE
      ),
      filterable = FALSE,
      columns = list(
        eno_rank = colDef(
          name = "",
          maxWidth = 55
        ),
        headshot_url = colDef(
          maxWidth = 50,
          align =  "center",
          name = "",
          cell = reactablefmtr::embed_img(height = 40, width = 40),
        ),
        name = colDef(
          cell = function(value, index) {
            # Render as a link
            url <- pitching_tbl$fangraphs_stats_url[index]
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
        stuff_plus = colDef(
          name = "STUFF+",
          align = "center",
          maxWidth = 60,
          style = color_scales(pitching_tbl, colors = c("#fd84a9", "white", "#42c2ca"))
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
    widths = c(2, 10),
    list(
      team_filter,
      stuff_filter,
      location_filter,
      pitching_filter
    ),
    pitching_tbl_react
  )
}

# Generate Auction
generate_sp_auction_crosstalk()
