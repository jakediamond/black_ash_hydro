# 
# Purpose: To compile all water table data for new sites
# Coder: Jake Diamond
# Date: November 15, 2018
# 

# Set Working directory
setwd("C:/Users/diamo/Dropbox/Projects/EAB/Data/HydroData")
# setwd("E:/Dropbox/Dropbox/Projects/EAB/Data/HydroData")

# Load Libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Function that processes an individual .csv
process_sheet <- function(filename) {
  df <- read.csv(filename, skip = 1, header = TRUE)
  df[, 1] <- NULL
  colnames(df) <- c("datetime", "P", "temp")
  df$datetime <- as.POSIXct(df$datetime, 
                            format = "%m/%d/%y %r", 
                            tz = "UTC")
  # Makes sure pressure is in Pa and not kPa
  df$P <- ifelse(df$P / 1000 > 1, 
                 df$P, 
                 df$P * 1000) 
  df
  }

# Function that reads in all annual levellogger sheets in the folder
read_all <- function(filenames) {
  for (i in 1:length(filenames)) {
    workbook <- filenames[i]
    data <- process_sheet(workbook)
    type <- unlist(strsplit(workbook, "[/]"))[[3]]
    type <- unlist(strsplit(type, "[_]"))[[1]]
    site <- unlist(strsplit(workbook, "[_]"))[[2]]
    year <- as.numeric(substr(unlist(
      strsplit(workbook, "[_]"))[[3]],
      start = 1, stop = 4)
      )
    setting <- substr(site, start = 1, stop = 1)
    number <- as.numeric(substr(site, start = 2, stop = 2))
    df <- mutate(data, 
                 year = year,
                 type = type, 
                 site = site, 
                 setting = setting, 
                 number = number)
    if(i == 1) {
      totalp <- df
      } else{
      totalp <- rbind(totalp, df)
      }
    }
  return(totalp)
  }

# Get filenames and create dataframe of all baro-corrected 
# levellogger data for all sites for all years
filenames <- paste("Water Level Data/New Sites/", 
                   list.files("Water Level Data/New Sites"), 
                   sep = "")
df <- read_all(filenames)

# Add barometric pressure for D2 and S2, which share gages with D3 and S1, respectively
df <- df %>% 
  filter(type == "Baro", site == "D3") %>% 
  mutate(site = "D2", number = 2) %>% 
  bind_rows(df)

df <- df %>% 
  filter(type == "Baro", site == "S1") %>% 
  mutate(site = "S2", number = 2) %>% 
  bind_rows(df)

# Clean Data (pressure is in Pascals)
pressure <- df %>% 
  select(-temp) %>%
  tidyr::spread(key = type, value = P) %>% 
  mutate(wtpress = Level - Baro) %>%
  filter(Level != 0)

pressure <- df %>%
  filter(type == "Level") %>%
  select(-type, -P) %>%
  right_join(pressure)
  
# Convert pressure to water level in meters
pressure$rho <- 1000 * (1 - (pressure$temp + 288.9414) / 
                          (508929.2 * (pressure$temp + 68.12963)) *
                          (pressure$temp - 3.9863)^2) # kg m^-3
g <- 9.8 # m s^-2
pressure$wtlevel <- pressure$wtpress / (pressure$rho * g)

# Read in sensor depths
sensors <- read.csv("MetaData/New Sites/sensor_depths.csv")

# Add well specs to dataframe, calculate water level, and add a "year" column
df2 <- sensors %>% 
  right_join(., pressure) %>%
  arrange(site, year, datetime)

# Account for string cutting in S2 because of mud (20 cm)
df2 <- df2 %>%
  mutate(sensor_depth.cm = ifelse(site == "S2" &
                                    between(datetime, 
                                            as.POSIXct("2017-08-11 09:30",
                                                            tz = "UTC"),
                                            as.POSIXct("2018-01-01 00:00",
                                                       tz = "UTC")),
                                  sensor_depth.cm + 20,
                                  sensor_depth.cm)
  )
  
# Account for cap falling off of D4 well, decreasing string length by 50.5 cm
df2 <- df2 %>%
  mutate(sensor_depth.cm = ifelse(site == "D4" &
                                    between(datetime, 
                                            as.POSIXct("2017-09-30 15:30",
                                                       tz = "UTC"),
                                            as.POSIXct("2018-01-01 00:00",
                                                       tz = "UTC")),
                                  sensor_depth.cm + 54.5,
                                  sensor_depth.cm)
  )

# Account for L1 well, decreasing string length by 17 cm
df2 <- df2 %>%
  mutate(sensor_depth.cm = ifelse(site == "L1" &
                                    between(datetime, 
                                            as.POSIXct("2017-08-10 11:00",
                                                       tz = "UTC"),
                                            as.POSIXct("2018-01-01 00:00",
                                                       tz = "UTC")),
                                  sensor_depth.cm + 17,
                                  sensor_depth.cm)
  )

# Account for string cutting in T3 because of mud (18 cm)
df2 <- df2 %>%
  mutate(sensor_depth.cm = ifelse(site == "T3" &
                                     between(datetime, as.POSIXct("2017-08-04",
                                                            tz = "UTC"),
                                             as.POSIXct("2018-01-01 00:00",
                                                        tz = "UTC")),
                                  sensor_depth.cm + 18,
                                  sensor_depth.cm)
  )

# Put the water level into meters
df2$waterlevel <- ifelse(df2$wtlevel >= 0,
                        df2$wtlevel + df2$sensor_depth.cm / 100,
                        NA)

# Plotting level data
plot <- ggplot(data = df2, aes(x = datetime, 
                               y = waterlevel, 
                               colour = as.factor(number))) +
  theme_bw() + theme(axis.title.x = element_blank(), 
                     axis.title.y = element_text(face = "bold", 
                                                 vjust = 0.6),
                     strip.text = element_text(face = "bold")) + 
  ylab("Water Level (m)") +
  scale_colour_manual(name = "Site Number",
                      breaks = c(1, 2, 3, 4),
                      values = c("black", "grey", "light blue", "blue")) +
  geom_point(alpha = 0.3,
             shape = 1) +
  facet_wrap(~setting, ncol = 1) +
  scale_y_continuous(limits = c(-1, 0.5),
                     breaks = seq(-1, 0.5, 0.5)) +
  scale_x_datetime(date_breaks = "3 months")
  
plot

write.csv(df2, file = "water_table_new_sites.csv")
