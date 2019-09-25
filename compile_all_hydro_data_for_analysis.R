#
# Purpose: To combine all water table and rainfall data for black ash sites
# Coder: Jake Diamond
# Date: November 25, 2019
#

# Set Working directory
# setwd("E:/Dropbox/Dropbox/Projects/EAB/Data/HydroData")
setwd("C:/Users/jake.diamond/Dropbox/Projects/EAB/Data/HydroData")

# Load libraries
library(tidyverse)
library(lubridate)

#### Only did this once, then saved as .rds file for easy replication ####

# Read in water table and rainfall data, 3 files
# This is the Phase 2 sites from 2011-2018
df <- read.csv("alldata_new_sites.csv")
# This is the Experimental (EAB) sites from 2011-2015
df2 <- read.csv("alldata_EAB.csv")
# This is the Experimental (EAB) sites from 2016-2018
df3 <- readRDS("Water Level Data/EAB Project/EAB_2016through2018")

# Only want three control sites from "EAB" data, and get rid of non-important columns
# If the water level is near the bottom of the well (i.e., within 3% of the well depth)
# Then we will call that value NA...these data are not reliable
# Don't want data for 2016 from this dataset because it was not QA/QC...
# that year is in the next data frame. Also B6co in 2011 was bad data
df2 <- df2 %>%
  mutate(waterlevel = ifelse(waterlevel < -0.97 * loggerdepth, 
                             NA,
                             waterlevel)) %>%
  dplyr::select(datetime, year, id, date, waterlevel, rain_m) %>%
  rename(rain_15min = rain_m,
         site = id) %>%
  dplyr::filter(site %in% c("b1co", "b3co", "b6co"),
         year != 2016,
         !(site == "b6co" & year == 2011)) %>%
  mutate(site = recode(site,
                       b1co = "L1",
                       b3co = "L2",
                       b6co = "L3"),
         waterlevel = waterlevel / 100,
         datetime = as_datetime(datetime),
         date = as_date(date)
  ) %>%
  droplevels()
# Same thing as above, but for years 2016-2018, the original file did not account
# for the distance between the top of the logger and the logger sensor (14.1 cm)
# so we need to subtract that from the measured level
df3 <- df3 %>%
  mutate(waterlevel = ifelse(year > 2015, 
                             waterlevel - 14.1,
                             waterlevel),
         waterlevel = ifelse(waterlevel < -0.97 * (depth + 14.1), 
                             NA,
                             waterlevel)) %>%
  dplyr::select(datetime, year, id, waterlevel) %>%
  mutate(date = date(datetime)) %>%
  rename(site = id) %>%
  dplyr::filter(site %in% c("b1co", "b3co", "b6co")) %>%
  mutate(site = recode(site,
                       b1co = "L1",
                       b3co = "L2",
                       b6co = "L3"),
         waterlevel = waterlevel / 100
  ) %>%
  droplevels()

# Rename Phase 2 site "L1" to L4 because now we call
# EAB controls from blocks 1, 3, and 6, L1, L2, and L3
df <- df %>%
  select(site, year, datetime, waterlevel, date, rain_15min, wtpress) %>%
  mutate(site = recode(site,
                       L1 = "L4"),
         datetime = as_datetime(datetime),
         date = as_date(date)) %>%
  droplevels()

# Join all dataframes
df <- bind_rows(df, df2, df3)
df$site <- as.factor(df$site)
# Save data
saveRDS(df, "all_black_ash_hydro_new")
