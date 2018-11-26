#
# Purpose: To calculate ET or phase 2 sites
# Coder: Jake Diamond
# Date: October 18, 2018
#
# Clear work space
rm(list = ls())  

# Load Libraries
library(dplyr)
library(ggplot2)
library(signal)

# Working directory
setwd("C:/Users/diamo/Dropbox/Projects/EAB/Data/HydroData")

# Load water table data and clean it up for unnecessary columns
df <- read.csv("alldata_new_sites.csv")
df <- df %>%
  dplyr::select(site, year, datetime, waterlevel, sensor_depth.cm) %>%
  mutate(datetime = as.POSIXct(datetime, 
                               format = "%Y-%m-%d %H:%M", 
                               tz = "UTC")) %>%
  rename(loggerdepth = sensor_depth.cm)

# Read in rr functions (sydata)
rr <- read.csv("rr_equations_v2.csv")

# Function to put low-pass filter on data for ease of use
lowpass_fun <- function(data, cutoff_frequency = 3) {
  names(data) <- tolower(names(data))
  # Need to remove all NAs for filter to be correct
  data$waterlevel <- ifelse(is.na(data$waterlevel), 
                            -data$loggerdepth, 
                            data$waterlevel)
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
  vec <- as.vector(data$waterlevel)
  filtered <- filtfilt(myfilter, vec)
  # Filtered data
  data$filtered <- filtered
  data <- data[with(data, order(datetime)), ]
  rem <- sr / cutoff
  data <- data[-c(1:rem, (nrow(data) - rem):nrow(data)),]
}

# Function to compute ET, not compensated by specific yield, 
# via White method (1932)
white_fun <- function(data) {
  #ET/Sy = S + h
  #S = WT_midnight.i - WT_midnight.(i+1)
  #h = water table dependent groundwater flow
  
  # Low-pass filter data to remove noise that confounds ET estimates
  wt <- lowpass_fun(data)
  
  # Prepare data for analysis
  wt$datetime <- as.POSIXct(wt$datetime, tz = "UTC") #UTC time zone 
  wt$date <- as.Date(wt$datetime, tz = "UTC")
  wt$hourmin <- as.numeric(strftime(wt$datetime, 
                        format = "%H%M", tz = "UTC"))
  dates <- as.Date(wt$datetime)
  
  # Detrend water table data to make hydraulic head of 
  # the recovery source constant
  # wt.dt = wt(t) - m.t*t - b.t
  # Then determine f(wt.dt) = dWT.dt/dt
  # Then h = Sy*[f(wt.dt(t)) + m.t]
  detrend_fun <- function(data){
    # use sliding window function to detect trends, 
    # window and step are in days
    slide_fun <- function(data, window, step){
      total <- nrow(data)
      interval <- as.numeric(data$datetime[2] - data$datetime[1])
      starts <- seq(from = 1, 
                    to = (total - (window * 24 * 60 / interval)), 
                    by = (step * 24 * 60 / interval))
      data$timeseq <- seq(from = 0, 
                          to = (nrow(data) - 1) * interval, 
                          by = interval)
      results <- data.frame(date = as.Date(character()),
                            m = numeric(), 
                            b = numeric(), 
                            pval = numeric(),
                            rsq = numeric(),
                            stringsAsFactors = FALSE)
      for(i in 1:length(starts)){
        x <- as.matrix(data[starts[i]:(starts[i] + 
                                         (window * 24 * 60 / interval)),
                            "timeseq"])
        y <- as.matrix(data[starts[i]:(starts[i] + 
                                         (window * 24 * 60 / interval)),
                            "filtered"])
        mod <- lm(y ~ x)
        m <- coef(mod)[2]
        b <- coef(mod)[1]
        pval <- anova(mod)$'Pr(>F)'[1]
        rsq <- summary(mod)$r.squared
        start_date <- data[starts[i], "date"]
        results[i, ] <- c(start_date, m, b, pval, rsq)
      }
      return(results)
    }
    window <- 3 # sliding window (in days) to determine trend
    step <- 1 # step size of the window (in days)
    # Apply sliding window function
    trends <- slide_fun(data, window = window, step = step)
    
    # Test to see if the trend changes from negative to positive
    # indicating a rain event
    trends$sign_test <- sign(trends$m) / sign(lag(trends$m))
    
    # If there was a change in trend, use the previous trend
    trends <- trends %>% 
      mutate(m.use = ifelse(sign_test == -1, 
                            lag(m), 
                            ifelse(lag(sign_test) == -1, 
                                   lag(m, 2), 
                                   m)))
    trends <- trends %>% 
      mutate(b.use = ifelse(sign_test == -1, 
                            lag(b), 
                            ifelse(lag(sign_test) == -1, 
                                   lag(b, 2), 
                                   b)))
    df <- merge(data, trends, by = "date", sort = FALSE)
    df <- df[order(df$datetime , decreasing = FALSE),]
    
    # Detrend data
    interval <- as.numeric(data$datetime[2] - data$datetime[1])
    df$timeseq <- seq(from = 0, 
                      to = (nrow(df) - 1)* interval, 
                      by = interval)
    df$dt <- df$filtered - 
      as.numeric(df$m.use) * (df$timeseq) - as.numeric(df$b.use)
    return(df)
  }
  
  # Function to get the dwt.dt/dt ~ wt.dt relationship 
  # (net groundwater inflow as a function of wt) 
  # to propograte throughout the day
  slope_fun <- function(night) {
    if (length(night$datetime) < 15 |
        all(is.na(night$dt))) {
      # Determine if enough data points
      results <- data.frame(NULL)
    } else {
      night <- night[order(night$datetime , decreasing = FALSE),]
      dwdt <- diff(night$dt)/diff(night$timeseq)
      mod <- lm(dwdt ~ night$dt[2:length(night$dt)])
      nightslope <- coef(mod)[2]
      nightb <- coef(mod)[1]
      pval <- anova(mod)$'Pr(>F)'[1]
      rsq <- summary(mod)$r.squared
      results <- data.frame(nightslope, nightb, pval, rsq)
    }
  }
  
  # Function to get midnight values
  mid_fun <- function(data) {
    index <- which(abs(data$hourmin - 0) == min(abs(data$hourmin - 0)))
    midnight <- data$hourmin[index]
    filtered <- data$filtered[index]
    data.frame(midnight, filtered)
  }
  
  # Detrend water table
  wt <- detrend_fun(wt)
  
  # Get dataframe of daily net groundwater inflow functions (r)
  nighttime_values <-
    wt[strftime(wt$datetime, format = "%H", tz = "UTC") %in%
         c("00", "01", "02", "03", "04", "05"), ]

  r.df <- nighttime_values %>% 
    group_by(date) %>% 
    do(slope_fun(.)) %>% 
    as.data.frame()
  
  # Merge those daily net groundwater inflow functions with the data
  df.dt <- merge(wt, r.df, by = "date", sort = FALSE)
  df.dt <- df.dt[order(df.dt$datetime , decreasing = FALSE),]
  
  # Calculate net groundwater inflow for each timestamp, 
  # then sum by day (h)
  df.dt$r <- df.dt$dt * df.dt$nightslope + df.dt$nightb + df.dt$m.use
  h <- df.dt %>% 
    group_by(date) %>% 
    summarize(h = sum(r))

  meanwt <- wt %>% 
    group_by(date) %>% 
    summarise(avg_wt = mean(filtered))
  
  S_results <- wt %>% 
    group_by(date) %>% 
    do(mid_fun(.)) %>% 
    as.data.frame() %>% 
    mutate(S = filtered - lead(filtered)) %>% as.data.frame()
  
  et_df <- S_results %>% 
    dplyr::select(date, S) %>% 
    left_join(., h) %>% 
    left_join(., meanwt) %>%
    mutate(ET = S + h)
}

# Function to compute specific yield based on rain:rise functions for each site
sy_fun <- function(ETdata, rr_funs = rr) {
  combine <- merge(ETdata, rr_funs)
  wt <- combine$avg_wt[1] / 100 # meters
  a <- combine$a[1]
  b <- combine$b[1]
  sy.min <- combine$minsy[1]
  sy.max <- combine$maxsy[1]
  sy.ini <- a * exp(b * wt)
  if (sy.ini < sy.min) {
    sy <- sy.min
  } else {
    if (sy.ini > sy.max) {
      sy <- sy.max
    } else{
      sy <- sy.ini
    }
  }
}

# Apply functions to get first estimates of ET
et <- df %>% 
  group_by(site, year) %>% 
  do(white_fun(.))
  
write.csv(et, "rawet_new_sites_EAB.csv")

Sy <- et %>% 
  group_by(block, treatment, date) %>% 
  do(Sy = sy_fun(.))
et <- merge(et, Sy)
et$ETSy <- et$ET * unlist(et$Sy)

# Need to clean data
# First, look at rainfall
df$date <- as.Date(df$date)
rainsummary <- df %>% 
  group_by(block, treatment, date) %>% 
  summarise(rain = sum(rain_m, na.rm = TRUE))

et <- merge(et, rainsummary)
et_norain <- et[et$rain == 0,]

# Next, look at water level: 
# don't want ET if waterlevel is hovering near the bottom of the well
et_clean <- df %>% 
  dplyr::select(block, treatment, year, loggerdepth) %>% 
  distinct() %>% 
  inner_join(et_norain)
et_clean$loggerdepth <- -et_clean$loggerdepth
et_clean2 <- et_clean %>%
  dplyr::filter(avg_wt > 0.975 * loggerdepth)
et_clean$Sy <- unlist(et_clean$Sy)
write.csv(et_clean, "et_all_EAB.csv")

# Compare data loss monthly distribution
library(lubridate)
et$month <- month(et$date)
et_clean$month <- month(et_clean$date)
et_clean2$month <- month(et_clean2$date)
rain.days <- anti_join(et, et_clean, by = c("block", "treatment", "date"))
ggplot(data = rain.days) + geom_histogram(aes(month)) #+ facet_wrap()
wl.days <- anti_join(et_clean, et_clean2, by = c("block", "treatment", "date"))
ggplot(data = wl.days) + geom_histogram(aes(month)) #+ facet_wrap()





# Derivative of filtered, sign change test, want at least 2 between 5am and 8pm, no more than 3, timing of sign change
# sc <- df %>% group_by(block, treatment, year) %>% do(lowpass_fun(., cutoff_frequency = 5)) %>% as.data.frame() %>%
#   mutate(der = lead(filtered) - filtered) %>% select(block, treatment, date, der) %>% group_by(block, treatment, date) %>%
#   mutate(signchange = sign(lead(der))/sign(der)) %>% group_by(block, treatment, date) %>%
#   summarize(signcount = sum(signchange[which(signchange<0)]))
# 
# et_cleanest <- merge(et_clean, sc)
# et_cleanest <- et_cleanest[et_cleanest$signcount > -3, ]
# et_cleanest <- et_cleanest[et_cleanest$signcount < -1, ]
# et_cleanest$Sy2 <- unlist(et_cleanest$Sy)
# et_cleanest$Sy <- NULL
# write.csv(et_cleanest, "et2.csv")

pet <- read.csv("RAWS_PET.csv")
pet$Date <- as.Date(pet$Date, format = "%m/%d/%Y")
pet$year <- NULL
names(pet) <- tolower(names(pet))
pet$pet <- pet$pet_mm / 10

# et_cleanest <- merge(et_cleanest, pet, by = "date")
# et_cleanest$et.pet <- et_cleanest$ETSy / et_cleanest$pet
# write.csv(et_cleanest,"et_pet.csv")

et_clean <- merge(et_clean, pet, by = "date")
et_clean$et.pet <- et_clean$ETSy / et_clean$pet
et_clean$year <- et_clean$year.x
et_clean$year.x <- NULL
et_clean$year.y <- NULL
write.csv(et_clean,"et_pet.csv")

# sum <- et_cleanest %>% 
#   group_by(block, treatment, year) %>% 
#   filter(ETSy > 0, et.pet < 3) %>%
#   summarize(sum = sum(ETSy))

sum <- et_clean %>%
  group_by(block, treatment, year) %>%
  dplyr::filter(ETSy > 0, et.pet < 3) %>%
  dplyr::summarize(sum = sum(ETSy))

# df2 <- read.csv("et_pet.csv")
# df2$date <- as.Date(df2$date, format = "%Y-%m-%d")

# df2$julian <- as.numeric(format(df2$date, "%j"))
et_clean$julian <- as.numeric(format(et_clean$date, "%j"))

library(ggplot2)
time_plot <-
  ggplot(data = et_clean %>% dplyr::filter(block == "b1"),
         aes(x = julian, y = et.pet, colour = treatment)) +
  theme_bw() + theme(axis.title.x = element_blank(),
                     axis.title.y = element_text(face = "bold", vjust = 0.6),
                     strip.text = element_text(face = "bold")) +
  ylab("ET (cm)") +
  geom_line(stat = "smooth", method = "loess", size = 2) + geom_point() + facet_wrap(~ year)  + ylim(0,2)
time_plot
