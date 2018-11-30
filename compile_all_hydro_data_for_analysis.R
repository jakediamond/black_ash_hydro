#
# Purpose: To combine all water table and rainfall data for black ash sites
# Coder: Jake Diamond
# Date: November 25, 2019
#

# Set Working directory
# setwd("E:/Dropbox/Dropbox/Projects/EAB/Data/HydroData")
setwd("C:/Users/diamo/Dropbox/Projects/EAB/Data/HydroData")

# Load libraries
library(dplyr)
library(lubridate)

#### Only did this once, then saved as .rds file for easy replication ####

# Read in water table and rainfall data, 3 files
df <- read.csv("alldata_new_sites.csv")
df2 <- read.csv("alldata_EAB.csv")
df3 <- readRDS("Water Level Data/EAB Project/EAB_2016through2018")

# Only want three sites from "EAB" data, and get rid of non-important columns
df2 <- df2 %>%
  mutate(waterlevel = ifelse(waterlevel < -0.95 * loggerdepth, 
                             NA,
                             waterlevel)) %>%
  select(datetime, year, id, date, waterlevel, rain_m) %>%
  rename(rain_15min = rain_m,
         site = id) %>%
  filter(site %in% c("b1co", "b3co", "b6co"),
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

df3 <- df3 %>%
  mutate(waterlevel = ifelse(waterlevel < -0.95 * depth, 
                             NA,
                             waterlevel)) %>%
  select(datetime, year, id, waterlevel) %>%
  mutate(date = date(datetime)) %>%
  rename(site = id) %>%
  filter(site %in% c("b1co", "b3co", "b6co")) %>%
  mutate(site = recode(site,
                       b1co = "L1",
                       b3co = "L2",
                       b6co = "L3"),
         waterlevel = waterlevel / 100
  ) %>%
  droplevels()

df <- df %>%
  select(site, year, datetime, waterlevel, date, rain_15min, wtpress) %>%
  mutate(site = recode(site,
                       L1 = "L4"),
         datetime = as_datetime(datetime),
         date = as_date(date)) %>%
  droplevels()

# Join two dataframes
df <- bind_rows(df, df2, df3)
df$site <- as.factor(df$site)
saveRDS(df, "all_black_ash_hydro")
