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
library(lubridate)
library(tidyr)

# Working directory
setwd("C:/Users/jake.diamond/Dropbox/Projects/EAB/Data/HydroData")

# Load filtered water table data and clean it up for unnecessary columns
df <- readRDS("all_black_ash_hydro_filtered")
df_raw <- readRDS("all_black_ash_hydro_new")

# Read in rr functions (sydata)
rr <- read.csv("rain_rise_equations_all_sites.csv")

# Load PET data
pet <- read.csv("RAWS_PET.csv")
pet$Date <- as.Date(pet$Date, format = "%m/%d/%Y")
pet$year <- NULL
names(pet) <- tolower(names(pet))
pet$pet <- pet$pet_mm / 1000 #m


# Define functions --------------------------------------------------------
# Function to compute ET, not compensated by specific yield, 
# via White method (1932)
white_fun <- function(data) {
  #ET/Sy = S + h
  #S = WT_midnight.i - WT_midnight.(i+1)
  #h = water table dependent groundwater flow
  
  # Low-pass filter data to remove noise that confounds ET estimates
  wt <- data
  
  # Prepare data for analysis
  wt$hourmin <- as.numeric(strftime(wt$datetime, 
                                    format = "%H%M", 
                                    tz = "UTC"))
  wt$date <- lubridate::date(wt$datetime)
  dates <- wt$date
  
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
                                   m)
      ),
      b.use = ifelse(sign_test == -1, 
                     lag(b), 
                     ifelse(lag(sign_test) == -1, 
                            lag(b, 2), 
                            b)
      )
      )
    
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
    mutate(S = filtered - lead(filtered)) %>% 
    as.data.frame()
  
  et_df <- S_results %>% 
    dplyr::select(date, S) %>% 
    left_join(., h) %>% 
    left_join(., meanwt) %>%
    mutate(ET = S + h)
}

# Function to compute specific yield based on rain:rise functions for each site
sy_fun <- function(ETdata, rr_funs = rr) {
  combine <- merge(ETdata, rr_funs)
  wt <- combine$avg_wt[1] # meters
  a <- combine$a[1]
  a2 <- combine$a2[1]
  int <- combine$int[1]
  sy.min <- combine$minsy[1]
  sy.max <- combine$maxsy[1]
  sy.ini <-  a * wt + a2 * wt^2 + int
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


# Apply functions to get first estimates of ET ----------------------------
et <- df %>% 
  group_by(site, year) %>% 
  do(white_fun(.)) %>%
  ungroup()

Sy <- et %>% 
  group_by(site, date) %>% 
  do(Sy = sy_fun(.)) %>%
  unnest()

et <- left_join(et, Sy)
et$ETSy <- et$ET * et$Sy


# Clean ET data -----------------------------------------------------------
# First, look at rainfall
rainsummary <- df_raw %>% 
  group_by(site, date) %>% 
  summarise(rain = sum(rain_15min, na.rm = TRUE))

et <- left_join(et, rainsummary)
et_norain <- et[et$rain == 0,]

# Next, remove days when S = 0, or negative ET, too big ET
et_clean <- et_norain %>%
  dplyr::filter(S > 0.001,
                ETSy > 0,
                ETSy < 0.015)
# Save data
saveRDS(et_clean, "et_all_sites")


# Look at ET/PET ----------------------------------------------------------
et_clean <- merge(et_clean, pet, by = "date")
et_clean$et.pet <- et_clean$ETSy / et_clean$pet
et_clean$year <- et_clean$year.x
et_clean$year.x <- NULL
et_clean$year.y <- NULL
write.csv(et_clean,"et_pet.csv")

# Sum of ET, summarize with rain
sum <- et_clean %>%
  group_by(site, year) %>%
  dplyr::filter(ETSy > 0, et.pet < 3) %>%
  dplyr::summarize(sum = sum(ETSy)) %>%
  left_join(rainsummary %>%
              mutate(year = year(date)) %>%
              group_by(site, year) %>%
              dplyr::summarize(rain = sum(rain))) %>%
  mutate(et_ppt = sum / rain)

# Some plotting -----------------------------------------------------------
et_clean$julian <- as.numeric(format(et_clean$date, "%j"))

# Time plot of ET
(ggplot(data = et_clean,
       aes(x = julian, 
           y = ETSy * 100, 
           colour = as.factor(year))) +
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", vjust = 0.6),
        strip.text = element_text(face = "bold")) +
  ylab("ET (cm)") +
    scale_color_viridis_d(name = "Year") +
  geom_line(stat = "smooth", method = "loess", size = 2, color = "red") + 
  geom_point(alpha = 0.2) + 
  facet_wrap(~ site)) %>%
  ggsave(filename = "Figures/ET_time_series.tiff",
         device = "tiff",
         dpi = 300)

# Time plot of ET/PET
(ggplot(data = et_clean,
        aes(x = avg_wt, 
            y = et.pet, 
            colour = julian)) +
    theme_bw() + 
    ylab("ET/PET") +
    xlab("Mean daily water level (m)") +
    scale_y_continuous(limits = c(0, 3)) + 
    # scale_x_continuous(breaks = seq(-0.7, 0.2, 0.5)) +
    scale_color_viridis_c(name = "Julian date") +
    geom_line(stat = "smooth", method = "loess", size = 2, color = "red") +
    geom_point(alpha = 0.3) + 
    facet_wrap(~ site, scales = "free_x")) %>%
  ggsave(filename = "Figures/ET_PET_watertable.tiff",
         device = "tiff",
         dpi = 300)

# Monthtly plot of ET
(ggplot(data = et_clean %>%
          mutate(month = month(date)),
        aes(x = month, 
            y = ETSy * 100)) +
    theme_bw() + 
    ylab("Mean daily ET (cm/d)") +
    xlab("Month") +
    stat_summary(fun.y = mean, geom = "point") + 
    stat_summary(fun.data = mean_cl_boot, 
                 geom = "errorbar", width = 0.2) + 
    geom_hline(data = et_clean %>%
                 group_by(site) %>%
                 summarize(mean = mean(ETSy, na.rm = TRUE) * 100),
               aes(yintercept = mean),
               linetype = "dashed", 
               color = "red") +
    facet_wrap(~ site, scales = "free_x")) %>%
  ggsave(filename = "Figures/ET_monthly.tiff",
         device = "tiff",
         dpi = 300)
