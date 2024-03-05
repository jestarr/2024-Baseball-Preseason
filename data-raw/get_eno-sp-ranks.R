# Get Eno Starting Pitcher Rankings
library(starrbaseball)
# first get sheet names to know what to use
get_eno_sp_rankings(sheet_names = TRUE)

eno <- get_eno_sp_rankings(sheet = "ranks mar4",sheet_names = FALSE)


readr::write_csv(eno2, "data-raw/2024_eno-sarris-pitching-ranks.csv")