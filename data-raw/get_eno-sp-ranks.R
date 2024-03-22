# Get Eno Starting Pitcher Rankings
library(starrbaseball)
# first get sheet names to know what to use
get_eno_sp_rankings(sheet_names = TRUE)

eno <- get_eno_sp_rankings(sheet = "WorkingRanksMar21", sheet_names = FALSE)


readr::write_excel_csv(eno, "data-raw/2024_eno-sarris-pitching-ranks.csv")
