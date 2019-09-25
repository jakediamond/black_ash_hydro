#
# Purpose: To summarize trends in water level data for black ash wetlands
# Coder: Jake Diamond
# Date: September 9, 2019
#

# Set Working directory
# setwd("E:/Dropbox/Dropbox/Projects/EAB/Data")
# setwd("C:/Users/diamo/Dropbox/Projects/EAB/Data")
setwd("C:/Users/jake.diamond/Dropbox/Projects/EAB/Data")

# Load libraries
library(tidyverse)
library(scales)
library(egg)
library(viridis)
library(lubridate)

# Load and clean data -----------------------------------------------------
# Load data for only 2016 and up
df <- readRDS("HydroData/all_black_ash_hydro_new") %>%
  dplyr::filter(year > 2015)

# Remove known outliers (T3 and D1 bottom out, S1 is taken out in 2018)
# Also, someone removed the cap on L4 in early September 2018, so there is
# missing data there
df <- df %>%
  mutate(waterlevel = ifelse((year == 2017 &
                                site == "T3" &
                                between(date, 
                                        as.Date("2017-07-29"), 
                                        as.Date("2017-09-24"))),
                             NA,
                             waterlevel),
         waterlevel = ifelse((site == "S1" &
                                datetime > ymd_hms("2018-10-12 18:45:00")),
                             NA,
                             waterlevel),
         waterlevel = ifelse((site == "D1" &
                                waterlevel < -0.38),
                             NA,
                             waterlevel),
         waterlevel = ifelse((!(site %in% 
                                  c("D4", "L1", "L2", "L3")) &
                                wtpress < 200),
                             NA,
                             waterlevel)
  )

# Read in elevation data, only get data for wells
elev <- read.csv("clean_hummocks_and_valpts_detrend.csv") %>%
  dplyr::filter(point == "well") %>%
  dplyr::select(site, z)

# Hydrologic analysis -----------------------------------------------------
# Calculate daily mean water table and daily sum of rain
# For bottomed out wells, assume the water level is 10cm below minimum
df_day <- df %>% 
  left_join(elev) %>%
  group_by(site, year) %>% 
  mutate(
    wt.fill = ifelse(is.na(waterlevel),
                     min(waterlevel, na.rm = TRUE) - 0.1,
                     waterlevel)
  ) %>%
  ungroup() %>%
  group_by(site, date) %>%
  dplyr::summarize(meanwt = mean(wt.fill, na.rm = TRUE),
                   rainsum = sum(rain_15min, na.rm = TRUE))

# Calculate means, medians, 80%/20% quantile
# Make sure 
hydro_summary <- df_day %>%
  group_by(site) %>%
  dplyr::summarize(mean = mean(meanwt, 
                               na.rm = TRUE),
                   median = median(meanwt, 
                                   na.rm = TRUE),
                   quant_80 = quantile(meanwt, probs = 0.8, 
                                       na.rm = TRUE),
                   quant_20 = quantile(meanwt, probs = 0.2, 
                                       na.rm = TRUE),
                   variation = var(meanwt, 
                                   na.rm = TRUE)
                   ) %>%
  mutate(quant_ratio = quant_80 / quant_20)

# Calculate annual rainfall and the number of events
rain <- df_day %>%
  mutate(year = year(date)) %>%
  group_by(site, year) %>%
  summarize(ppt = sum(rainsum, na.rm = TRUE),
            count = sum(rainsum != 0, na.rm = TRUE),
            countall = n(),
            frac = count / countall)

# Read in ET data
et <- readRDS("HydroData/et_all_sites") %>%
  dplyr::filter(year > 2015)

# Calculate average daily ET, taking out bad data
et_mean <- et %>%
  dplyr::filter(ETSy < 2,
         ETSy > 0) %>%
  group_by(site) %>%
  summarize(et = mean(ETSy, na.rm = TRUE))

# Calculate hydroperiod
hydroperiod <- df_day %>%
  mutate(year = year(date)) %>%
  left_join(elev) %>%
  group_by(site, year) %>%
  dplyr::summarize(hydroperiod = sum(meanwt > 0, 
                                     na.rm = TRUE)) %>%
  group_by(site) %>%
  dplyr::summarize(avghydro = mean(hydroperiod),
                   sdhydro = sd(hydroperiod))

# Add hydroperiod to results
hydro_summary <- left_join(hydro_summary, hydroperiod) %>%
  left_join(et_p)

# Get into long format for plotting
results_l <- hydro_summary %>%
  gather(key = "metric", value = "value", -site)

# Save
write.csv(hydro_summary, "average_wt_info_new_sites_same_years.csv")

# Plotting ----------------------------------------------------------------
# Some summary plots ------------------------------------------------------
# Quick boxplot without considering any NA issues 
(ggplot(data = df_day,
        aes(x = site,
            y = meanwt)) +
   geom_jitter() + 
   geom_boxplot() +
   stat_summary(fun.y = mean, 
                geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
                width = .75, color = "red") +
   theme_bw() +
   ylab("Mean daily water table (m)") +
   xlab("")) %>%
  ggsave(filename = "HydroData/Figures/boxplot_no_NA_action.tiff",
         device = "tiff",
         dpi = 300)

# Now same, but considering NA issues, NA = minimum depth - 10cm
(df %>% 
  left_join(elev) %>%
  group_by(site, year) %>% 
  mutate(
    wt.fill = ifelse(is.na(waterlevel),
                     min(waterlevel, na.rm = TRUE) - 0.1,
                     waterlevel)
  ) %>%
  ungroup() %>%
  group_by(site, date) %>%
  dplyr::summarize(meanwt = mean(wt.fill, na.rm = TRUE)) %>%
  ggplot(aes(x = site,
             y = meanwt)) +
  geom_jitter() + 
  geom_boxplot() +
  stat_summary(fun.y = mean, 
               geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = .75, color = "red") +
    theme_bw() +
    ylab("Mean daily water table (m)") +
    xlab("")) %>%
  ggsave(filename = "HydroData/Figures/boxplot_NA_min_minus_10cm.tiff",
         device = "tiff",
         dpi = 300)

# Quick plot of all hydro data
ggplot(data = results_l,
       aes(x = site,
           y = value)) + 
  geom_col() + 
  theme_bw() + 
  facet_wrap(~metric, scales = "free")

# Plot daily water table across sites
# Get all data into one year for plotting
df_day <- df_day %>%
  ungroup() %>%
  mutate(date2015 = ymd(format(df_day$date, "2015-%m-%d")),
         year = year(date))

# Then, get datetime limits for plots
limsd <- as.Date(c("2015-05-07",
                   "2015-11-07"), 
                 format = "%Y-%m-%d")

# Plot
(ggplot(data = mutate(df_day, year = year(date))) +
  geom_line(size = 1.5,
            alpha = 0.7,
            aes(x = date2015,
                y = meanwt *100,
                colour = as.factor(year),
                group = year)) +
  facet_wrap(~site, nrow = 3) + 
  geom_text(data = et_mean,
            aes(x = as.Date("2015-07-20",
                            format = "%Y-%m-%d"),
                y = -100,
                label = paste0("ET=",
                               format(round(et * 100,
                                            digits = 2),
                                      nsmall = 2),
                               "cm/d"
                               )
                ),
            show.legend = FALSE) +
  scale_x_date(date_breaks = "2 month",
               date_labels = "%b"
               , limits = limsd
               ) +
  theme_bw() +
  scale_colour_viridis_d(name = "Year",
                         direction = -1) +
  theme(axis.title.x = element_blank(),
        legend.position = c(0.75, 0.15),
        legend.direction = "vertical",
        legend.box.margin = margin(c(0,0,0,0), unit='cm'),
        legend.margin = margin(c(0,0,0,0), unit = "cm"),
        panel.grid = element_blank()
  ) +
  guides(color = guide_legend(ncol = 2)) + 
  ylab("Relative daily water table (cm)") +
  geom_hline(yintercept = 0, 
             linetype = "dashed",
             size = 1,
             color = "grey30",
             alpha = 0.7)) %>%
  ggsave(
       filename = "HydroData/Figures/mean_daily_wt_all_sites_ET_P_same_years.tiff",
       device = "tiff",
       width = 8,
       height = 6,
       dpi = 300)


