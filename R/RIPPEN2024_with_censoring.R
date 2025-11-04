library(invgamma)
library(parallel)
library(ggplot2)
library(plotly)
library(teamcolors)
source("./R/gatherData.r")
source("./R/kickCoef.r")

# Gather data from 2009-2024 seasons
# Check if data has already been gathered
data_file_path <- "./data/total09-24_pbp_data.csv"
first_time <- file.exists(data_file_path)
if(first_time == TRUE){
  # Gather data from scratch
  print("Data file not found. Gathering data from scratch...")
  nfl <- gatherData()  
  write.csv(nfl, data_file_path, append = FALSE, row.names = FALSE)
  print(sprintf("Data gathered and saved to %s", data_file_path))
} else {
  # Load existing data
  nfl <- read.csv(file = data_file_path)
  print("Data file found and loaded.")
}

# Calculate kick coefficients
kickCoef <- kickCoef(nfl)

# Gather pass plays
passPlays <- gatherPassPlays(nfl)

