library(tidyverse)
library(baseballstarR)

get_historical_player_tbl <- function(league_name = "sammyfbl") {
  # Get Player ID Map
  mlb_players <- baseballstarR::historical_player_table_w_prospects
  
  # Subset Columns
  mlb_players <- mlb_players %>% 
    mutate(dateBirth = lubridate::ymd(dateBirth)) %>% 
    mutate(agePlayer = lubridate::as.period(lubridate::interval(start = dateBirth, end = lubridate::today()))$year) %>% 
    dplyr::select(
      dplyr::matches("yearSeason"),
      dplyr::matches("isActive|isTopProspect|hasDebuted"),
      dplyr::matches("^namePlayerMLB$|namePlayerCBS|namePlayerFG|namePlayerNFBC"),
      dplyr::matches("idPlayer"),
      dplyr::matches("slugTeam|nameTeam"),
      dplyr::matches("^slugPosition|positionCBS|^bats$|^throws$|height"),
      dplyr::matches("weight"),
      dplyr::matches("dateBirth|agePlayer"),
      everything()
    ) %>%
      arrange(desc(yearSeason))

  
  # Get CBS Players
  cbs_players <- baseballstarR::cbs_players_table(league_name = league_name)
  
  # Subset CBS Players to necessary columns
  cbs_players <- cbs_players %>% 
    select(one_of(c("isFreeAgent","idPlayerCBS","nameTeamCBS","contractPlayer","salaryPlayer","slugPositionCBS","slugPositionsEligible","articleHeadlineCBS"))) %>% 
    mutate(slugPositionCBS = ifelse(slugPositionCBS %in% c("LF", "CF", "RF"), "OF", slugPositionCBS))
  
   player_tbl <- mlb_players %>% 
    left_join(cbs_players)
  return(player_tbl)
}

fg_auction_calc <- function(teams = 12,
                            budget = 270,
                            stats_type = "bat",
                            projection_source = "steamer",
                            bat_split = 60,
                            rest_of_season = FALSE) {
  
  if (rest_of_season == TRUE) {
    if (projection_source == "steamer") {
      projection_source <- glue::glue("{projection_source}r")
    } else {
      projection_source <- glue::glue("r{projection_source}")
    }
  }

  # zips doesn't appear
  if (projection_source == "zips") {
    url <- glue::glue("https://www.fangraphs.com/api/fantasy/auction-calculator/data?teams={teams}&lg=MLB&dollars={budget}&mb=1&mp=20&msp=5&mrp=5&players=&proj=zips&split={bat_split}&rep=0&drp=0&pp=C,SS,2B,3B,OF,1B&pos=1,1,1,1,4,1,0,0,0,2,6,3,0,5,0&sort=&view=0&type={stats_type}&points=c|1,2,3,4,5|0,1,2,3,4")
  } else {
    url <- glue::glue("https://www.fangraphs.com/api/fantasy/auction-calculator/data?teams={teams}&lg=MLB&dollars={budget}&mb=1&mp=20&msp=5&mrp=5&type={stats_type}&players=&proj={projection_source}&split={bat_split}&points=c|1,2,3,4,5|0,14,2,3,4&rep=0&drp=0&pp=C,SS,2B,3B,OF,1B&pos=1,1,1,1,4,1,0,0,0,2,6,3,0,5,0&sort=&view=0")
  }

  resp <- httr::GET(url)

  # Get JSON URL
  resp_url <- resp$url

  # URL Message
  print(glue::glue("Getting {projection_source} projections.  Scraping data from: {resp_url}"))

  httr::warn_for_status(resp)
  json <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), flatten = TRUE)

  # bind to df
  data <- json$data

  # add column to identify which projections
  data <- data %>%
    dplyr::mutate(
      projection_source = dplyr::case_when(
        projection_source == "atc" ~ "ATC",
        projection_source == "fangraphsdc" ~ "Depth Charts",
        projection_source == "fan" ~ "Fans",
        projection_source == "steamer" ~ "Steamer",
        projection_source == "steamer600" ~ "Steamer600",
        projection_source == "thebat" ~ "THE BAT",
        projection_source == "thebatx" ~ "THE BAT X",
        projection_source == "zips" ~ "ZiPS",
        projection_source == "rfangraphsdc" ~ "Depth Charts (RoS)",
        projection_source == "rsteamer" ~ "Steamer (RoS)",
        projection_source == "steamer600" ~ "Steamer600",
        projection_source == "rthebat" ~ "THE BAT (RoS)",
        projection_source == "rthebatx" ~ "THE BAT X (RoS)",
        projection_source == "rzips" ~ "ZiPS (RoS)",
        projection_source == "2023" ~ "2023 YTD",
        projection_source == "2022" ~ "2022 YTD",        
        projection_source == "2021" ~ "2021 YTD",
        projection_source == "2020" ~ "2020 YTD",
        projection_source == "2019" ~ "2019 YTD"
      ), .before = 1
    ) %>%
    dplyr::select(-Name)
    ) # remove HREF Name column

  data <- data %>%
    rename(
      player_name = PlayerName,
      dollars = Dollars,
      adjusted = Adjusted,
      cost = Cost
    )
}

# ---- Auction Calc ROS ----

# function to generate table of auction calculator results with projection sources
auction_calc_tbl <- function(projection_sources = c("atc", "fangraphsdc", "steamer", "thebat", "thebatx"),
                             teams = 12,
                             budget = 270,
                             stats_type = "bat",
                             bat_split = 60,
                             compact = TRUE) {
  df <- projection_sources %>%
    purrr::set_names() %>%
    purrr::map(., ~ fg_auction_calc(
      projection_source = .x,
      stats_type = stats_type,
      teams = teams,
      budget = budget,
      bat_split = bat_split
    )) %>%
    dplyr::bind_rows() %>%
    distinct() # remove dupes
  
  if (compact == TRUE) {
    df <- df %>%
      select(sourceProjection, idPlayerFG, playerName, ADP, dollars)
  } else {
    df
  }
  return(df)
}


# summary table with summary stats for projection sources by player: mean, min, max
auction_calc_value_summary <- function(projection_sources = c("atc", "fangraphsdc", "steamer", "thebat", "thebatx"),
                                       teams = 12,
                                       budget = 270,
                                       stats_type = "bat",
                                       bat_split = 60,
                                       compact = TRUE){
  
  df <- auction_calc_tbl(projection_sources ,
                         stats_type = stats_type,
                         compact = compact)
  
  # summary table
  df <-  df %>% 
    group_by(idPlayerFG, playerName) %>% 
    summarise(across(c(dollars), list(mean = mean, min = min, max = max, sd = sd))) %>% 
    mutate(across(starts_with("dollars"), round, 1)) %>% 
    arrange(desc(dollars_mean)) %>% 
    ungroup()
}




# get eno sarris stuff + spreadsheet
get_stuff_plus_sheet <- function(sheet = "Spring Training 2022 (thru 4/2)", sheet_names = FALSE) {
  
  googlesheets4::gs4_deauth()
  # pitcher report google sheets ID
  ssid <- "1AE1dNnudwRS6aLhWA1SArp1GoviUeHNcASXxtm3Le9I"
  
  if (sheet_names == TRUE) {
    # list names of worksheets you can call
    googlesheets4::as_sheets_id(ssid)
  } else {
    # save to df
    df <- googlesheets4::read_sheet(ssid, sheet = sheet)
    
    df <- df %>%
      dplyr::mutate(across(where(is.list), as.character))
    return(df)
  }

  
}






























get_cbs_positions <- function(season = 2019){
  if (!"cbs_token" %>% exists()) {
    baseballstarR::assign_cbs_token()
  }
  base_url <- "http://api.cbssports.com/fantasy/league/stats?version=3.1"
  # TODO: Fix this
  cbs_team_id_slug <- ""
  
  
  # TODO: Come up with point and period slugs
  params <-
    list(
      player_status = "all",
      position = "",
      timeframe = 2019, # effective_year
      period = "", # time_period
      stats_type = "stats",
      team_id = cbs_team_id_slug,
      team_type = "",
      point = "",
      response_format = "JSON",
      access_token = cbs_token
    )
  
  slug_param <-
    baseballstarR:::.generate_param_slug(params = params)
  
  url <-
    glue::glue("{base_url}&{slug_param}") %>% as.character()
  
  json <-
    url %>%
    jsonlite::fromJSON(simplifyDataFrame = TRUE, flatten = TRUE)
  
  data <-
    json$body$league_stats$players %>%
    dplyr::as_tibble()
  
  df <-
    data %>% 
    select(id, name, starts_with("GP")) %>% 
    mutate_at(vars(starts_with("GP")), as.numeric)
  
  # make long
  df <- df %>% 
    pivot_longer(cols = starts_with("GP"),
                 names_to = "pos",
                 names_prefix = "GP",
                 values_to = "G",
                 values_drop_na = TRUE)
  
  df <- df %>% 
    # recode DH to U
    mutate(pos = ifelse(pos == "DH", "U", pos)) %>% 
    # elibile games filter & positons
    filter(G > 20 & 
             pos %in% c("U", "1B", "2B", "3B", "SS", "OF")) %>% 
    arrange(pos) %>% 
    group_by(id, name) %>% 
    summarize(elig_pos = str_c(pos, collapse=",")) %>% 
    ungroup() %>% 
    mutate(elig_pos = ifelse(stringr::str_detect(elig_pos, "U"), elig_pos, glue::glue("{elig_pos},U"))) %>% 
    arrange(name)
  
  # name fixes
  df <- df %>% 
    rename(idPlayerCBS = id,
           namePlayerCBS = name,
           slugPositionsEligible = elig_pos)
  
  return(df)
}

cbs_players_table <- function() {
  # pull JSON from players search endpoint function
  json <- baseballstarR:::cbs_players_search_endpoint()
  
  # if CBS Players Search Endpoint doesn't return players, use Wildcards Endpoint as an alternate method
  if (json$statusCode == 500) {
    print(glue::glue("CBS Players Search Endpoint isn't working.  Using Wildcards endpoint to get data"))
    
    # player_status <- c("free_agents", "owned")
    
    player_status <- "free_agents"
    
    # Map over Player Status on Wildcards library(Endpoint
    data <- player_status %>%
      map(., ~ baseballstarR:::cbs_wildcards_endpoint(.x)) %>%
      map_dfr(., ~ pluck(.x, "body", "wildcards", "players"))
    
    
    data <- data %>%
      baseballstarR:::rename_player_columns(data_source = "cbs")
    
    
    positions_df <- get_cbs_positions()
    
    
    df <- data %>%
      left_join(positions_df)
    
    df <- df %>% 
      mutate(slugPositionCBS = ifelse(slugPositionCBS %in% c("LF", "RF", "CF"), "OF", slugPositionCBS),
             slugPositionsEligible = ifelse(is.na(slugPositionsEligible), slugPositionCBS, slugPositionsEligible)
      )
  }
  
  data <- json$body$players
  
  # unnest eligible positions columns
  data <- data %>%
    dplyr::mutate(eligible_positions = purrr::map_chr(eligible_positions, ~ stringr::str_c(.x, collapse = ", "))) %>%
    tibble::as_tibble()
  
  data <- data %>%
    rename_player_columns(data_source = "cbs")
  
  # select columns
  df <- data %>%
    dplyr::mutate(
      dateWaiversUntil = as.Date.POSIXct(dateWaiversUntil)
    ) %>%
    dplyr::select(-dplyr::one_of("nameFirst", "nameLast", "idPlayerElias", "linkPlayerCBS", "photo", "inPlayerPool")) %>%
    dplyr::select(-dplyr::matches("^throws|^age|^number|^bats|slugTeamFG"))
  
  df <-
    df %>%
    dplyr::select(
      dplyr::matches("isFreeAgent"),
      dplyr::matches("waivers"),
      dplyr::matches("^is"),
      dplyr::matches("idPlayer"),
      dplyr::matches("^namePlayerCBS$"),
      dplyr::matches("slugPosition"),
      dplyr::matches("slugTeam"),
      dplyr::matches("nameTeam"),
      dplyr::matches("contract|salary"),
      dplyr::matches("url"),
      dplyr::matches("headline"),
      dplyr::everything()
    ) %>%
    dplyr::distinct()
  return(df)
}


calculate_player_value <- function(df, 
                                   num_teams = 13,
                                   auction_budget = 270,
                                   bat_pct = .59,
                                   pit_pct = (1-bat_pct),
                                   num_bat = 11,
                                   num_pit = 9,
                                   stats_type = "bat"){
  # calculations from inputs
  total_lg_budget <- num_teams * auction_budget
  lg_bat_budget <- total_lg_budget * bat_pct
  lg_pit_budget <- total_lg_budget * (1 - bat_pct)
  total_bat_drafted <- num_teams * num_bat
  total_pit_drafted <- num_teams * num_pit
  bat_budget_allocated <- lg_bat_budget - total_bat_drafted
  pit_budget_allocated <- lg_pit_budget - total_pit_drafted
  
  # get historical standings for SGP Calculation Later
  standings <- readRDS(here::here("data", "historical_standings_2012_2019(sammyfbl).rds"))
  
  # get 2019 stats to calculate averages for ratio stats
  prior_season_stats <- readRDS(here::here("data", "stats_2019(sammyfbl).rds"))
  
  # Pull in SGP using Historical Standings Data
  # TODO: Look into removing top two, and bottom two from SGP
  sgp <- baseballstarR::calculate_sgp(standings, season = 2019, weights = c(.5, .3, .2))
  
  # filter SGP table to only include Weighted SGP & remove unnecessary columns
  sgp <- sgp %>%
    filter(typeData == "Weighted") %>%
    select(-yearSeason) %>%
    # pivot longer to join to long projection df below
    pivot_longer(-typeData, names_to = "typeStat", values_to = "SGPStat")
  
  if (stats_type == "bat") {
    # subset batting stats
    prior_season_stats <- prior_season_stats %>%
      select(idTeamCBS, nameTeamCBS, PA, AB, H, BB, HP, SF, OBP)
    
    # get average stats per player
    avg_stats <- prior_season_stats %>%
      # average per team
      summarize_if(is.numeric, ~ mean(.)) %>%
      # average per player
      mutate_at(vars("PA", "AB", "H", "BB", "HP", "SF"), ~ . / num_bat) %>%
      # ratio category
      mutate(OBP = (H + BB + HP) / (AB + BB + HP + SF))
    
    
    # create primary position column for multi position players 
    # looked at fangraphs auction calculator to decide order of value
    # positions listed in order of replacement value
    df <- df %>% 
      mutate(calcPosition = case_when(
        stringr::str_detect(slugPositionsEligible, "C") ~ "C",
        stringr::str_detect(slugPositionsEligible, "2B") ~ "2B",
        stringr::str_detect(slugPositionsEligible, "1B") ~ "1B",
        stringr::str_detect(slugPositionsEligible, "OF") ~ "OF",
        stringr::str_detect(slugPositionsEligible, "SS") ~ "SS",
        stringr::str_detect(slugPositionsEligible, "3B") ~ "3B",
        stringr::str_detect(slugPositionsEligible, "U") ~ "U",
        TRUE ~ slugPositionsEligible
      )  
      )
    
    # subset and filter df for Value Calculations using SGP
    value_tbl <- df %>%
      filter(PA > 50, !is.na(id), !is.na(PA), !is.na(calcPosition)) %>%
      mutate(
        numeratorOBP =
          ((((((avg_stats$OBP * avg_stats$PA) * (num_bat - 1))) + H + BB) /
              (((num_bat - 1) * avg_stats$PA) + AB + BB)) - avg_stats$OBP)
      ) %>%
      # select columns that are part of SGP
      select(sourceProjection,
             id,
             calcPosition,
             R,
             HR,
             RBI,
             SB,
             OBP = numeratorOBP
      )
  }
  
  if (stats_type == "pit") {
    # subset pitching stats
    pitching_stats <- c("INN", "ER", "HA", "BBI", "W", "SV", "SO", "ERA", "WHIP")
    
    prior_season_stats <- prior_season_stats %>%
      select(idTeamCBS, nameTeamCBS, !!pitching_stats)
    
    # get average stats per player
    avg_stats <- prior_season_stats %>%
      # average per team
      summarize_if(is.numeric, ~ mean(.)) %>%
      # average per player
      mutate_at(vars("INN", "ER", "HA", "BBI", "W", "SV", "SO"), ~ . / num_pit) %>%
      # ratio category
      mutate(
        ERA = (ER * 9) / INN,
        WHIP = (BBI + HA) / INN
      )
    
    df <- df %>% 
      mutate(calcPosition = case_when(
        id == "660271" ~ "SP",
        TRUE ~ slugPositionCBS)  
      )
    
    
    #  filter df for Value Calculations using SGP
    value_tbl <- df %>%
      filter(IP > 10, !is.na(id), !is.na(IP), !is.na(slugPositionCBS)) 
    
    # Calculate ratio stats replacment player levels for SGP
    # https://www.smartfantasybaseball.com/2013/03/create-your-own-fantasy-baseball-rankings-part-5-understanding-standings-gain-points/
    value_tbl <- value_tbl %>% 
      mutate(ERA = ((avg_stats$ER * (num_pit - 1) + ER) * 9 / 
                      ((avg_stats$INN * (num_pit - 1) + IP)) - avg_stats$ERA),
             WHIP = (
               (((avg_stats$HA + avg_stats$BBI) * (num_pit - 1)) + H + BB) /
                 ((avg_stats$INN * (num_pit - 1)) + IP) - avg_stats$WHIP)
      )
    
    # Select Columns
    scoring_cats_pit <- c("W", "SV", "SO", "ERA", "WHIP")
    
    value_tbl <- value_tbl %>% 
      select(sourceProjection, id, calcPosition, !!scoring_cats_pit)
    
  }
  
  # Pivot Longer for SGP Calculations
  value_tbl_long <- value_tbl %>%
    pivot_longer(c(-sourceProjection, -id, -calcPosition),
                 names_to = "typeStat",
                 values_to = "valueStat"
    )
  # join SGP to Values
  value_tbl_long <- value_tbl_long %>%
    left_join(sgp %>% select(typeStat, SGPStat))
  
  # calculate SGP's for stats
  value_calc <- value_tbl_long %>%
    mutate(
      SGP = valueStat / SGPStat,
      typeData = "SGP"
    )
  
  # make wide to join to original data later on
  value_calc <- value_calc %>%
    # remove unnecessary columns before pivot
    select(
      sourceProjection,
      id,
      calcPosition,
      typeData,
      typeStat,
      SGP
    ) %>%
    pivot_wider(
      names_from = c(typeStat, typeData),
      values_from = SGP
    ) %>%
    mutate(totalSGP = rowSums(select(., ends_with("SGP"))))
  
  # ranks
  if (stats_type == "bat") {
    # position ranks
    ranks <- value_calc %>%
      group_by(sourceProjection, calcPosition) %>%
      mutate(slugPositionRank = rank(-totalSGP)) %>%
      ungroup() 
    
    # util ranks for players that are util, or not starters
    util_ranks <- ranks %>% 
      group_by(sourceProjection) %>% 
      # create custom filter to rank Util players with filters and maintain original DF
      mutate(
        indexUtil = calcPosition %in% c("C", "1B", "2B", "3B", "SS") & slugPositionRank > 13 |
          calcPosition == "OF" & slugPositionRank > 52 |
          calcPosition == "U",
        utilRank = NA_real_,
        utilRank = replace(utilRank, indexUtil, rank(desc(totalSGP[indexUtil])))
      ) %>%
      ungroup() %>%
      select(-indexUtil)
    
    # rank bench
    bench_ranks <- util_ranks %>%
      filter(!is.na(utilRank)) %>%
      group_by(sourceProjection) %>% 
      mutate(
        indexBench = utilRank > 26,
        benchRank = NA_real_,
        benchRank = replace(benchRank, indexBench, rank(desc(totalSGP[indexBench])))
      ) %>%
      ungroup() %>% 
      select(-indexBench)
    
    # join util and bench ranks to all other ranks
    ranks <- ranks%>% left_join(util_ranks) %>% left_join(bench_ranks)
    
    # add descriptive columns 
    ranks <- ranks %>% 
      mutate(rosterSpot = case_when(
        calcPosition %in% c("C", "1B", "2B", "3B", "SS") & slugPositionRank <= 13 ~ "starter",
        calcPosition == "OF" & slugPositionRank <= 52  ~ "starter",
        utilRank <= 26 ~ "util",
        benchRank <= 26 ~ "bench",
        TRUE ~ "undrafted"
      ))
    
    # calculate replacement players
    # Replacement Level Players
    replacement_tbl <- ranks %>%
      # get all rostered players 
      filter(rosterSpot == "starter" | rosterSpot == "util" ) %>% 
      group_by(sourceProjection, calcPosition) %>%
      summarize(totalSGPRepl = min(totalSGP)) %>% 
      ungroup() %>% 
      filter(calcPosition != "U")
    
    util_tbl <- replacement_tbl %>% 
      group_by(sourceProjection) %>% 
      summarize(totalSGPRepl = max(totalSGPRepl)) %>% 
      ungroup() %>% 
      mutate(position1 = "U")
    
    # bind util to replacement tbl
    replacement_tbl <- replacement_tbl %>% 
      bind_rows(util_tbl)
    
    # add replacement level columns to table
    value_calc <- ranks %>%
      left_join(replacement_tbl) %>%
      group_by(sourceProjection) %>% 
      mutate(
        totalSGPOverRepl = totalSGP - totalSGPRepl,
        totalSGPRank = rank(-totalSGPOverRepl, ties.method = "first"),
        totalSGPUseful = ifelse(totalSGPRank <= total_bat_drafted, totalSGPOverRepl, 0)
      ) %>% 
      ungroup()
    
    # get useful SGP value
    useful_sgp_final <- value_calc %>%
      group_by(sourceProjection) %>% 
      filter(totalSGPRank == num_teams * num_bat) %>%
      rename(totalSGPUsefulFinal = totalSGPUseful) %>% 
      ungroup()
    
    # join useful SGP back to df
    value_calc <- value_calc %>%
      left_join(useful_sgp_final %>% select(sourceProjection, totalSGPUsefulFinal)) %>% 
      mutate(
        totalSGPOverFinal = totalSGPOverRepl - totalSGPUsefulFinal  )
    
    # get draftable SGP
    sgpDraftableBat <- value_calc %>%
      filter(totalSGPOverFinal > 0) %>%
      group_by(sourceProjection) %>% 
      summarize(sum = sum(totalSGPOverFinal)) %>% 
      ungroup() %>% 
      mutate(dollars_per_bat = bat_budget_allocated / sum)
    
    # final SGP calc
    value_calc <- value_calc %>%
      left_join(sgpDraftableBat) %>% 
      mutate(valuePlayer = totalSGPOverFinal * dollars_per_bat + 1)
  }
  
  # ranks
  if (stats_type == "pit") {
    # position ranks
    ranks <- value_calc %>%
      group_by(sourceProjection, calcPosition) %>%
      mutate(slugPositionRank = rank(-totalSGP)) %>%
      ungroup() 
    
    # add descriptive columns 
    ranks <- ranks %>% 
      mutate(rosterSpot = case_when(
        calcPosition == "SP" & slugPositionRank <= 78~ "starter",
        calcPosition == "RP" & slugPositionRank <= 39  ~ "starter",
        calcPosition == "SP" & (between(slugPositionRank, 78, 102)) ~ "bench",
        calcPosition == "RP" & (between(slugPositionRank, 39, 52)) ~ "bench",
        TRUE ~ "undrafted"
      ))
    
    # calculate replacement players
    # Replacement Level Players
    replacement_tbl <- ranks %>%
      # get all rostered players 
      filter(rosterSpot == "starter") %>% 
      group_by(sourceProjection, calcPosition) %>%
      summarize(totalSGPRepl = min(totalSGP)) %>% 
      ungroup()
    
    # add replacement level columns to table
    value_calc <- ranks %>%
      left_join(replacement_tbl) %>%
      group_by(sourceProjection) %>% 
      mutate(
        totalSGPOverRepl = totalSGP - totalSGPRepl,
        totalSGPRank = rank(-totalSGPOverRepl, ties.method = "first"),
        totalSGPUseful = ifelse(totalSGPRank <= total_pit_drafted, totalSGPOverRepl, 0)
      ) %>% 
      ungroup()
    
    # get useful SGP value
    useful_sgp_final <- value_calc %>%
      group_by(sourceProjection) %>% 
      filter(totalSGPRank == num_teams * num_pit) %>%
      rename(totalSGPUsefulFinal = totalSGPUseful) %>% 
      ungroup()
    
    # join useful SGP back to df
    value_calc <- value_calc %>%
      left_join(useful_sgp_final %>% select(sourceProjection, totalSGPUsefulFinal)) %>% 
      mutate(
        totalSGPOverFinal = totalSGPOverRepl - totalSGPUsefulFinal  )
    
    # get draftable SGP
    sgpDraftablePit <- value_calc %>%
      filter(totalSGPOverFinal > 0) %>%
      group_by(sourceProjection) %>% 
      summarize(sum = sum(totalSGPOverFinal)) %>% 
      ungroup() %>% 
      mutate(dollars_per_pit = pit_budget_allocated / sum)
    
    # final SGP calc
    value_calc <- value_calc %>%
      left_join(sgpDraftablePit) %>% 
      mutate(valuePlayer = totalSGPOverFinal * dollars_per_pit + 1)
  }
  
  values <- value_calc %>% 
    select(sourceProjection, id, valuePlayer, totalSGPRank, slugPositionRank)
  
  df <- df %>% left_join(values) %>% arrange(sourceProjection, -valuePlayer)
  
  # column order
  df <- df %>% 
    select(matches("^id"),
           matches("name|value"),
           everything())
  
  return(df)
}


create_player_capsule <- function(season = 2019, stats_type = "bat") {
  # Create sequence of most recent season plus 2 Prior Seasons
  seasons <- seq(season - 2, season)

  if (stats_type == "bat") {
    pos <- "np"
  }

  if (stats_type == "pit") {
    pos <- "all"
  }

  full_season <-
    seasons %>%
    furrr::future_map_dfr(~ get_fg_leaderboard(season = .x, stats_type = stats_type, pos = pos, split_type = "Full Season"), .progress = TRUE)

  # df of most current season 1st and 2nd Half Splits
  half_seasons <-
    furrr::future_map_dfr(c("1st Half", "2nd Half"), ~ get_fg_leaderboard(season, stats_type = stats_type, pos = pos, split_type = .x), .progress = TRUE)

  # bind season data together
  seasons_df <- bind_rows(full_season, half_seasons)


  # Get DF of splits for vs Lefty and vs Righty Data for current season
  vs_splits_df <- furrr::future_map_dfr(c("vs L", "vs R"), ~ get_fg_leaderboard(season, stats_type = stats_type, pos = pos, split_type = .x, columns_type = "Advanced"), .progress = TRUE)

  if (stats_type == "bat") {
    # Select Columns for Pivot
    vs_splits_df <- vs_splits_df %>%
      select(tidyselect::any_of(c("typeSplit", "namePlayerFG", "idPlayerFG", "wRCPlus")))

    # pivot wider
    vs_splits_df <- vs_splits_df %>%
      mutate(typeSplit = stringr::str_replace(typeSplit, " ", "")) %>%
      pivot_wider(
        names_from = typeSplit,
        values_from = wRCPlus,
        names_prefix = "wRCPlus"
      )
  }

  if (stats_type == "pit") {
    # Select Columns for Pivot
    vs_splits_df <- vs_splits_df %>%
      select(tidyselect::any_of(c("typeSplit", "namePlayerFG", "idPlayerFG", "xFIP")))

    # pivot wider
    vs_splits_df <- vs_splits_df %>%
      mutate(typeSplit = stringr::str_replace(typeSplit, " ", "")) %>%
      pivot_wider(
        names_from = typeSplit,
        values_from = xFIP,
        names_prefix = "xFIP"
      )
  }

  # Join vs splits to season data
  df <- seasons_df %>% left_join(vs_splits_df)


  # Mutate and Select columns
  if (stats_type == "bat") {
    # select columns
    seasons_df <- seasons_df %>%
      select(typeSplit,
             idPlayerFG,
             namePlayerFG,
             yearSeason,
             teamName,
             age,
             PA,
             AB,
             R,
             HR,
             RBI,
             SB,
             AVG,
             OBP,
             SLG,
             ISO,
             wRCPlus,
             pctBB,
             pctK,
             pctContact,
             pctSwingOutside,
             pctGB,
             pctFB,
             pctPU,
             pctHRtoFB)
  }

  if (stats_type == "pit") {
    df <- df %>%
      mutate(pctPU = pctIFFB * pctFB, .after = pctFB) %>%
      select(typeSplit,
             idPlayerFG,
             namePlayerFG,
             yearSeason,
             teamName,
             age,
             IP,
             GS,
             W,
             SV,
             ERA,
             WHIP,
             SIERA,
             xFIP,
             K9,
             BB9,
             HR9,
             pctBB,
             pctK,
             pctKminusBB,
             pctBall,
             pctStrikeSwinging,
             veloFourSeamFastballPI,
             pctGB,
             pctFB,
             pctPU)
  }
  return(df)
}


