###############################################################
#Purpose: To calculate rain:rise for new sites
#Coder: Jake Diamond
#Date: September 25, 2019
###############################################################
# Working directory
setwd("C:/Users/jake.diamond/Dropbox/Projects/EAB/Data/HydroData")

# Load Libraries
library(tidyverse)
library(lubridate)

# Read in data
df <- readRDS("all_black_ash_hydro_new")
df <- dplyr::rename(df, rain_m = rain_15min)

# Rain:Rise function
rr_fun <- function(data) {
  data <- data[with(data, order(datetime)), ]
  data <- data[!is.na(data$rain_m), ]
  # Create empty matrix to fill
  nrows <- nrow(data)
  firstrain <- min(which(!data$rain_m == 0))
  lastrain <- max(which(!data$rain_m == 0))
  rr_mat <- as.data.frame(matrix(NA, 
                                 nrow = (lastrain - firstrain + 1), 
                                 ncol = 11))
  colnames(rr_mat) <-
    c(
      "DateTime",
      "Stage",
      "AverageStage",
      "InitialStage",
      "Rain",
      "Rise",
      "RainRise",
      "RiseLag15",
      "RainRiseLag15",
      "RiseLag30",
      "RainRiseLag30"
    )
  rr_mat$DateTime <- as.POSIXct(rr_mat$DateTime)
  
  # Adds columns to combined water table and rain 
  # dataframe for counts and cumulative depths of rain events
  data$count <- rep(NA, nrow(data))
  data$rainsum <- rep(NA, nrow(data))
  
  # Loops through the combined data set and calculates cumulative rain
  # lengths and cumulative rain depths per event
  # This loop does not separate a rain event even if it has an
  # interval's worth break in rainfall
  for (i in firstrain:lastrain) {
    if (i - 1 == 0) {
      data$count[i] <- 1
      data$rainsum[i] <- data$rain_m[i]
    } else{
      if (data$rain_m[i] != 0 && is.na(data$count[i - 1])) {
        data$count[i] <- 1
      } else{
        if (data$rain_m[i] != 0) {
          data$count[i] <- data$count[i - 1] + 1
        } else{
          if (data$rain_m[i - 1] != 0 && data$rain_m[i + 1] != 0) {
            data$count[i] <- data$count[i - 1] + 1
          }
        }
      }
      if (!is.na(data$count[i])) {
        data$rainsum[i] <- sum(data$rain_m[(i - data$count[i] + 1):i])
      }
    }
  }
  
  # Take out interception from rain 
  # (comment out if first time running, so can get raw rr)
  # Interception for all sites is 1 mm leaf off and 2 mm leaf on
  data <- data %>%
    mutate(month = month(datetime),
           leaf = ifelse(month %in% c(1, 2, 3, 4, 10, 11, 12),
                         0, 1),
           interception = ifelse(leaf == 1, 0.002, 0.001),
           rainsum = ifelse(rainsum - interception > 0,
                           rainsum - interception,
                           0)
    )
  
  # Preps data in case first data is during rain event
  if (firstrain == 1) {
    firstna <- min(which(is.na(data$count)))
    data <- data[-(1:(firstna - 1)),]
    firstrain <- min(which(!data$rain_m == 0))
    lastrain <- max(which(!data$rain_m == 0))
  }
  
  #Calculate rain:rise
  for (i in (firstrain - 1):lastrain) {
    if (data$rain_m[i] != 0) {
      event_length <- data$count[i]
      rise <- data$waterlevel[i] - data$waterlevel[i - event_length]
      rain <- data$rainsum[i]
      rr <- rain / rise
      lag15_rise <-
        data$waterlevel[i + 1] - data$waterlevel[i - event_length + 1]
      lag30_rise <-
        data$waterlevel[i + 2] - data$waterlevel[i - event_length + 2]
      lag15_rr <- rain / lag15_rise
      lag30_rr <- rain / lag30_rise
      stage <- data$waterlevel[i]
      avg_stage <- mean(data$waterlevel[(i - event_length):i])
      initial_stage <- data$waterlevel[i - event_length]
      stage_date <- as.POSIXct(data$datetime[i], tz = "UTC")
      calc_data <- c(
        stage,
        avg_stage,
        initial_stage,
        rain,
        rise,
        rr,
        lag15_rise,
        lag15_rr,
        lag30_rise,
        lag30_rr
      )
      rr_mat[(i - (firstrain - 1)), 2:11] <- calc_data
      rr_mat[(i - (firstrain - 1)), 1] <- stage_date
    }
  }
  
  clean <- na.omit(rr_mat)
  attr(clean$DateTime, "tzone") <- "UTC"
  
  #Adds EventID column
  clean$EventID <- rep(NA, nrow(clean))
  for (i in 1:nrow(clean)) {
    if (i == 1) {
      clean$EventID[i] <- 1
    } else{
      if (clean$InitialStage[i] == clean$InitialStage[i - 1]) {
        clean$EventID[i] <- clean$EventID[i - 1]
      } else{
        clean$EventID[i] <- clean$EventID[i - 1] + 1
      }
    }
  }
  
  #Adds a column to dataframe for the total rainfall for each event. This is total rainfall - interception
  maxeventrain <- aggregate(Rain ~ EventID, clean, FUN = max)
  maxeventrain$MaxRain <- maxeventrain$Rain
  maxeventrain$Rain <- NULL
  clean <- merge(maxeventrain, clean, by = "EventID")
  
  flowrate <-
    6.7e-7 #m/s, equal to 2.25 inch per day - assumed to be the rate at which water
  #drains from site; used to estimate when site has fully "recovered" from previous rain
  
  #Adds a "keep" column to cleaned dataset to determine if the datapoint will be kept or discarded
  #based on whether or not each event is far enough removed in time from the previous event
  #based on the assumed flowrate, or drainage rate. This was more effective at keeping "good" storms than a simple,
  #say, 24 hour separation criterion.
  clean$keep <- rep(NA, nrow(clean))
  
  for (i in 1:nrow(clean)) {
    if (i == 1) {
      clean$keep[i] <- 1
    } else{
      newevent <-
        diff(clean$EventID)[i - 1] #Check to see if it is a new storm event
      if (newevent == 1) {
        if (clean$DateTime[i] < (clean$MaxRain[i - 1] / flowrate + clean$DateTime[i -
                                                                                  1])) {
          #"Recovery" test
          clean$keep[i] <- 0
        } else{
          clean$keep[i] <- 1
        }
      } else{
        if (clean$keep[i - 1] == 0) {
          clean$keep[i] <- 0
        } else{
          clean$keep[i] <- 1
        }
      }
    }
  }
  
  final <- subset(clean, keep == 1)
  final
}

# Execute rain:rise for each site
rr_all <- df %>%
  select(-wtpress) %>%
  na.omit() %>%
  group_by(site, year) %>%
  do(rr_fun(.))

# Write to csv, comment in if first time running
# write.csv(rr_all, "rr_raw_no_interception_new_sites.csv")

# Rain:rise by event, not individual 15-minute increments
rr_event <- rr_all %>%
  group_by(site, year, EventID) %>%
  dplyr::filter(Rain == MaxRain, Rain != 0)

# Use lags in case of negative rr
rr_event$clean_rr <- ifelse(rr_event$RainRise < 0,
                            ifelse(rr_event$RainRiseLag15 < 0,
                                   rr_event$RainRiseLag30,
                                   rr_event$RainRiseLag15),
                            rr_event$RainRise)

# Use lags in case of rr greater than 1
rr_event$clean_rr <- ifelse(rr_event$clean_rr > 1,
                            ifelse(rr_event$RainRiseLag15 > 1,
                                   rr_event$RainRiseLag30,
                                   rr_event$RainRiseLag15),
                            rr_event$clean_rr)

# Clean data
rr_event <- rr_event %>%
  dplyr::filter(clean_rr > 0, clean_rr < 1)

# Write to csv, comment in if first time running
# write.csv(rr_event, "rr_event_no_interception_new_sites.csv")

# Write to csv
write.csv(rr_event, "rr_event_new_sites.csv")

# Plot data, remove some outliers in Excel with visual inspection
ggplot(data = rr_event[rr_event$MaxRain > 0.005, ], 
       aes(x = AverageStage, 
           y = clean_rr,
           colour = year)) + 
  geom_point() + facet_wrap(~site) + 
  xlim(-0.5, 0.5) + geom_smooth() + ylim(0, 1.2)

# Read cleaned data for regression
df_reg <- read.csv("rr_event_new_sites_cleaned_for_regression.csv")

# Determine cutoffs for what data to consider as part of exponential regression
# Actually looks like it could be breakpoint...
rain_cutoff <- 0.005
df_reg <- df_reg[df_reg$MaxRain > rain_cutoff, ]

# Look at data
(ggplot(data = df_reg, 
       aes(x = AverageStage, 
           y = clean_rr, 
           colour = as.factor(year))) + 
  geom_point() + facet_wrap(~ site) + 
  xlim(-0.3, 0.2) + ylim(0, 1.2) +
  geom_smooth(
              method = "lm", 
              formula = y ~ x + I(x^2),
              color="red",
              se = FALSE, 
              linetype = 1) +
    theme_bw()+
    scale_color_viridis_d(name = "Year") +
    ylab("Rain:Rise") +
    xlab("Average stage (m)")) %>%
  ggsave(filename = "Figures/rain_rise.tiff",
         device = "tiff",
         dpi = 300)

# Function to get exponential Sy relationship for each site
quad_fun <- function(data) {
  library(broom)
  stage <- data$AverageStage
  stage2 <- data$AverageStage^2
  rr <- data$clean_rr
  mod <- lm(rr ~ stage + stage2)
  # a <- coef(mod)[1]
  # b <- coef(mod)[2]
  # RSS.p <- sum(residuals(mod)^2)  # Residual sum of squares
  # TSS <- sum((rr - mean(rr))^2)  # Total sum of squares
  # r2 <- 1 - (RSS.p/TSS)  # R-squared measure
  results <- tidy(mod)
}

# Apply Sy function, but don't consider S2 because it needs separate for each year
df_quad <- df_reg %>%
  group_by(site) %>%
  do(quad_fun(.))
  
# Write to .csv
write.csv(df_quad, "rain_rise_quadratic_fits_new_sites.csv")
