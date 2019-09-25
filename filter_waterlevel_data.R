# 
# Purpose: To smooth DO time-series for middle Loire, reduce outliers
# Author: Jake Diamond
# Date: September 5, 2019
# 

# Set working directory
# setwd("Z:/Loire_DO")
setwd("C:/Users/jake.diamond/Dropbox/Projects/EAB/Data")

# Load libraries
library(zoo)
library(signal)
library(imputeTS)
library(dygraphs)
library(tidyverse)
library(lubridate)

# Read in all DO data
df <- readRDS("HydroData/all_black_ash_hydro_new")

# Only get wt data and nest data by site and year, 
df_n <- df %>%
  select(site, year, datetime, waterlevel) %>%
  group_by(site, year) %>%
  nest()

# Remove original data to keep workspace clean
rm(df)

# Define functions --------------------------------------------------------
# Define lowpass filter function
# Can prescribe cutoff_frequency for low pass bandwidth (default 3 
# for 15 minute data)
lowpass_fun <- function(data, 
                        cutoff_frequency = 4) {
  # Re-interpolate all NAs so that there are none with Stineman method
  data$wl_an_int <- na_interpolation(data$waterlevel, option = "stine")
  # Order the data, just in case
  data <- data[with(data, order(datetime)),]
  # Sampling rate [s^-1]
  sr <- 1 / (as.numeric(data$datetime[2] - data$datetime[1]) * 60)
  # Nyquist frequency = half the sampling rate
  nyq <- sr / 2
  # Cutoff frequency (hours^-1)
  cutoff <- 1 / (cutoff_frequency * 60 * 60)
  # Normalized cutoff frequency for Butterworth filter
  W <- cutoff / nyq
  # Butterworth low-pass filter, digital, 2nd order
  myfilter <- butter(2, W, type = 'low', plane = 'z')
  # Forward-reverse filter to remove phase-shift 
  # associated with Butterworth filter (must be in vector-form)
  vec <- as.vector(data$wl_an_int)
  filtered <- filtfilt(myfilter, vec)
  # Filtered data
  data$filtered <- filtered
  data <- data[with(data, order(datetime)), ]
  rem <- sr / cutoff
  data <- data[-c(1:rem, (nrow(data) - rem):nrow(data)),]
}

# Apply functions ---------------------------------------------------------
# Clean the data and use the lowpass filter
df_n <- df_n %>%
  mutate(filt = map(data, lowpass_fun))

# Get all in one dataframe
df_filt <- unnest(df_n, filt)

# Plotting ----------------------------------------------------------------
# Interactive dygraphs
# Create a graphing function
graph_fun <- function(data, site = "sitename") {
    list(dygraph(data,
                 main = site,
                 width=800,height=200) %>%
           dyOptions(drawGrid = F,
                     useDataTimezone = TRUE) %>%
           dyAxis("y", label = "Water level (m)",
                  independentTicks = TRUE,
                  valueRange = c(-1.2, 0.5))  %>%
           dyRangeSelector())
}
# First need to get data in correct format, neseted
df_n <- df_filt %>%
  group_by(site) %>%
  nest() %>%
  mutate(ts = map(data, ~zoo::zoo(x = .[, c("waterlevel", "filtered")], 
                                  order.by = .$datetime)),
         p = map2(ts, site, ~graph_fun(.x, .y),
         ))

# Lookat graph and export
htmltools::browsable(htmltools::tagList(pluck(df_n, 4)))


# Save data ---------------------------------------------------------------
# Finally, add  back NAs for large chunks of missing data (i.e., >=1 day)
# because the filter can't adequately fill these gaps
df_filt <- df_filt %>%
  group_by(site, year) %>%
  mutate(na_check = 
           {res = rle(is.na(waterlevel));
           rep(res$values * res$lengths, res$lengths)},
         waterlevel = ifelse(na_check > 24,
                             NA,
                             filtered)) %>%
  ungroup()

# Save data
saveRDS(df_filt, "HydroData/all_black_ash_hydro_filtered")
