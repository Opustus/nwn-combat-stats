# update_dpr_overview.R
# Broad Overview Spreadsheet Update
# Updates the project's overview of damage per round AC ranges

source("R/nwn_dpr_core.R")
source("config/config.R")
library(googlesheets4)
library(dplyr)

gs4_auth()
builds <- read_sheet(SHEET_URL, sheet = "Builds")

# Process the data
results <- lapply(1:nrow(builds), function(i) {
  row <- builds[i, ]
  # Calculate mean damage for each bin using the core engine
  mean_damages <- sapply(AC_BINS, function(ac_seq) {
    mean(calculate_dpr_for_build(row, ac_seq))
  })
  return(mean_damages)
})

damage_matrix <- round(do.call(rbind, results))

range_write(
  ss = SHEET_URL,
  data = as.data.frame(damage_matrix),
  sheet = "Builds",
  range = "M2",
  col_names = FALSE
)
