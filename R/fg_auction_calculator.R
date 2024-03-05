
library(tidyverse)




fg_auction_calculator <- function(teams = 12,
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
  print(glue::glue("Getting {projection_source} Auction Calculator data.  Scraping data from: {resp_url}"))
  
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
    # remove HREF Name column
    dplyr::select(-Name)


data <- data %>%
  select(
    projection_source, playerid,
    team = Team, player_name = PlayerName, pos = POS,
    ADP, mRBI,
    mR,
    mSB,
    mHR,
    mOBP,
    PTS,
    aPOS,
    FPTS,
    dollars = Dollars,
    adjusted = Adjusted,
    cost = Cost,
    cPOS
  )

return(data)
}
