#
# Purpose: To compile rain data for new sites
# Coder: Jake Diamond
# Date: November 16, 2018
#

#Working directory
# setwd("E:/Dropbox/Dropbox/Projects/EAB/Data/HydroData")
setwd("C:/Users/diamo/Dropbox/Projects/EAB/Data/HydroData")

# Load Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# Function that processes an individual .csv
process_sheet_rain <- function(filename) {
  df <- read.csv(filename, skip = 1, header = TRUE)
  df[, 1] <- NULL
  colnames(df) <- c("datetime", "T", "Event")
  df$datetime <- as.POSIXct(df$datetime, 
                            format = "%m/%d/%y %r", 
                            tz = "UTC")
  df$eventrain <- ifelse(df$Event > 0, 0.2 / 1000, NA)

  # Turns rain data into time series object and sums rain over 15 minute increments. 
  # E.g., the value at 12:15 is the sum of rain from 12:00 to 12:14.
  rain <- df %>% 
    group_by(datetime = ceiling_date(datetime, unit = "15 minutes")) %>% 
    summarize(rain_15min = sum(eventrain, na.rm = TRUE))
  rain
}

# Function that reads in all rain data sheets in the folder
read_all_rain <- function(filenames) {
  for (i in 1:length(filenames)) {
    workbook <- filenames[i]
    data <- process_sheet_rain(workbook)
    site <- unlist(strsplit(workbook, "[_]"))[[2]]
    year <- as.numeric(substr(unlist(
      strsplit(workbook, "[_]"))[[3]],
      start = 1, stop = 4)
    )
    df <- mutate(data, 
                 year = year,
                 site = site)
    if(i == 1) {
      totalrain <- df
    } else{
      totalrain <- rbind(totalrain, df)
    }
  }
  return(totalrain)
}

filenames <- paste("Weather/New Sites/", 
                   list.files("Weather/New Sites"), sep = "")

rain <- read_all_rain(filenames)

write.csv(rain, "rain_new_sites.csv")
