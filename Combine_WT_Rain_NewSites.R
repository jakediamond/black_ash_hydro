#
# Purpose: To combine water table and rain data for new sites
# Coder: Jake Diamond
# Date: November 16, 2018
#

# Working directory
# setwd("E:/Dropbox/Dropbox/Projects/EAB/Data/HydroData")
setwd("C:/Users/diamo/Dropbox/Projects/EAB/Data/HydroData")

# Load Libraries
library(dplyr)

# Read in rain data
rain <- read.csv("rain_new_sites.csv")
rain$datetime <- as.POSIXct(rain$datetime, 
                            format = "%Y-%m-%d %H:%M:%S", 
                            tz = "UTC")
rain$X <- NULL
colnames(rain)[4] <- "rainsite"
rain$rainsite <- tolower(rain$rainsite)

# Read in water table data
wtdf <- read.csv("water_table_new_sites.csv", 
                 stringsAsFactors = FALSE)
wtdf$X <- NULL
wtdf$datetime <- as.POSIXct(wtdf$datetime, 
                            format = "%Y-%m-%d %H:%M:%S",
                            tz = "UTC")
wtdf$date <- as.Date(wtdf$datetime)
names(wtdf) <- tolower(names(wtdf))

# Define a rain site for each site and year
matchdf <- read.csv("site_rain_match.csv")
wtdf <- merge(wtdf, matchdf)

df <- wtdf %>% 
  group_by(rainsite, datetime) %>% 
  left_join(rain) %>% 
  arrange(site, datetime)

write.csv(df, "alldata_new_sites.csv")
