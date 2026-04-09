# plot_dpr_builds.R
# Plot DPR Builds
# Plots a graph in R for selected builds over AC range 35 through 70

source("R/nwn_dpr_core.R")
source("config/config.R")
library(googlesheets4)
library(dplyr)
library(tidyr)

gs4_deauth()
builds <- read_sheet(SHEET_URL, sheet = "Builds")

ac_curve <- 35:70

# Selecting builds by row indices, header's not included
target_indices <- c(1,2, 3) 
target_builds <- builds[target_indices, ]

# Build a matrix where each column = one build's DPR curve.
dpr_matrix <- sapply(1:nrow(target_builds), function(i) {
  row_data <- target_builds[i, ]
  
  calculate_dpr_for_build(row_data, ac_curve)
})

# Plot the graph!
colnames(dpr_matrix) <- target_builds$build

matplot(x = ac_curve, 
        y = dpr_matrix, 
        type = "l", 
        lwd = 3, 
        lty = 1, # Solid lines
        col = 1:ncol(dpr_matrix),
        xlab = "Enemy AC", 
        ylab = "Average Damage Per Round",
        main = "Build Damage Comparison",
        xaxt = 'n')

axis(side = 1, at = seq(30, 70, by = 5))

legend("topright", 
       legend = colnames(dpr_matrix), 
       col = 1:ncol(dpr_matrix), 
       lty = 1, 
       lwd = 3,
       cex = 0.7,
       bg = rgb(1,1,1,0.8),
       box.col = "gray")
