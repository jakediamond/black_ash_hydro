#
# Purpose: To plot water table across sites, and to summarize trends
# Coder: Jake Diamond
# Date: December 6, 2018
#

# Set Working directory
# setwd("E:/Dropbox/Dropbox/Projects/EAB/Data")
setwd("C:/Users/diamo/Dropbox/Projects/EAB/Data")

# Load libraries
library(tidyverse)
library(scales)
library(egg)
library(viridis)
library(lubridate)

# Load data
df <- readRDS("HydroData/all_black_ash_hydro")

# For later plot, make everything in the same year (2015)
df <- df %>%
  mutate(date2015 = ymd(format(df$date, "2015-%m-%d")),
         datetime2015 = ymd_hm(format(df$datetime, "2015-%m-%d %H:%M"))
  )

# Remove known outliers (T3 and D1 bottom out)
df <- df %>%
  mutate(waterlevel = ifelse((year == 2017 &
                                site == "T3" &
                                between(date2015, 
                                        as.Date("2015-07-29"), 
                                        as.Date("2015-09-24"))),
                             NA,
                             waterlevel),
         waterlevel = ifelse((site == "D1" &
                                waterlevel < -0.38),
                             NA,
                             waterlevel),
         waterlevel = ifelse((!(site %in% c("D4", "L1", "L2", "L3")) &
                                wtpress < 200),
                             NA,
                             waterlevel)
  )

# Read in elevation data for detrended surfaces
elev <- readRDS("elevation_data") %>%
  filter(point == "well") %>%
  select(site, z_mod, minpt)

# Calculate daily values
df_day <- df %>% 
  left_join(elev) %>%
  group_by(site, year, date2015) %>% 
  mutate(waterlevel_rel = waterlevel - z_mod) %>%
  dplyr::summarize(meanwt = mean(waterlevel, na.rm = TRUE),
                   sumrain = sum(rain_15min, na.rm = TRUE),
                   meanwt_rel = mean(waterlevel_rel, na.rm = TRUE))

# Hydrologic analysis -----------------------------------------------------
# Calculate means, medians, 80%/20% quantile
# Make sure 
results <- df_day %>%
  group_by(site) %>%
  dplyr::filter(date2015 < as.Date("2015-11-01"),
                date2015 > as.Date("2015-05-07")
                ) %>%
  mutate(
    wt.fill = ifelse(is.na(meanwt),
                     min(meanwt, na.rm = TRUE) - 0.1,
                     meanwt),
    wt.fill_rel = ifelse(is.na(meanwt_rel),
                         min(meanwt_rel, na.rm = TRUE) - 0.1,
                         meanwt_rel)
    ) %>%
  dplyr::summarize(mean = mean(wt.fill, 
                               na.rm = TRUE),
                   median = median(wt.fill, 
                                   na.rm = TRUE),
                   quant_80 = quantile(wt.fill, probs = 0.8, 
                                       na.rm = TRUE),
                   quant_20 = quantile(wt.fill, probs = 0.2, 
                                       na.rm = TRUE),
                   variation = var(wt.fill, 
                                   na.rm = TRUE),
                   mean_rel = mean(wt.fill_rel, 
                                   na.rm = TRUE),
                   median_rel = median(wt.fill_rel, 
                                       na.rm = TRUE),
                   quant_80_rel = quantile(wt.fill_rel, probs = 0.8, 
                                           na.rm = TRUE),
                   quant_20_rel = quantile(wt.fill_rel, probs = 0.2, 
                                           na.rm = TRUE),
                   variation_rel = var(wt.fill_rel, 
                                       na.rm = TRUE)) %>%
  mutate(quant_ratio = quant_80 / quant_20,
         quant_ratio_rel = quant_80_rel / quant_20_rel)

# Calculate Annual rainfall
rain <- df_day %>%
  group_by(site, year) %>%
  summarize(ppt = sum(sumrain, na.rm = TRUE),
            count = n())

# Read in ET data
et <- read.csv("HydroData/et_pet.csv")

# Calculate average daily ET, taking out bad data
et_mean <- et %>%
  filter(et.pet < 1.5,
         ETSy > 0) %>%
  group_by(site, year) %>%
  summarize(et = mean(ETSy, na.rm = TRUE))

# Determine ET/P ratio, estimating ET as average daily ET * number of days 
# P was measured
et_p <- left_join(et_mean, rain) %>%
  filter(ppt > 0) %>%
  mutate(ET = et * count,
         ET.P = ET / ppt) %>%
  filter(ET.P < 2) %>%
  summarize(ET.P_avg = mean(ET.P))

# Calculate hydroperiod
hydroperiod <- df_day %>%
  dplyr::filter(date2015 < as.Date("2015-11-01"),
                date2015 > as.Date("2015-05-07")) %>%
  left_join(elev) %>%
  group_by(site, year) %>%
  dplyr::summarize(hydroperiod_rel = sum(meanwt > minpt, 
                                         na.rm = TRUE),
                   hydroperiod = sum(meanwt > 0, 
                                     na.rm = TRUE)) %>%
  group_by(site) %>%
  dplyr::summarize(avghydro_rel = mean(hydroperiod_rel),
                   avghydro = mean(hydroperiod))

results <- left_join(results, hydroperiod) %>%
  left_join(et_p)

results.l <- results %>%
  gather(key = "metric", value = "value", -site)

ggplot(data = results.l,
       aes(x = site,
           y = value)) + 
  geom_col() + 
  theme_bw() + 
  facet_wrap(~metric, scales = "free")


write.csv(results, "average_wt_info_new_sites_v4.csv")

# Plotting ----------------------------------------------------------------
# Plot daily water table across sites
# First, get datetime limits for plots
limsd <- as.Date(c("2015-05-07",
                   "2015-11-07"), 
                 format = "%Y-%m-%d")

# Plot
p_wt <- ggplot(data = filter(df_day,
                             !(site %in% c("L4", "S1", "S2")))) +
  geom_line(size = 1.5,
            alpha = 0.7,
            aes(x = date2015,
                y = meanwt * 100,
                colour = factor(year),
                group = year)) +
  facet_wrap(~site, nrow = 3) + 
  geom_text(data = filter(et_p,
                          !(site %in% c("L4", "S1", "S2"))),
            aes(x = as.Date("2015-06-15",
                            format = "%Y-%m-%d"),
                y = -100,
                label = paste0("ET/P = ", format(round(ET.P_avg, 
                                                       digits = 2), 
                                                 nsmall = 2)
                               )
                ),
            show.legend = FALSE) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b",
               limits = limsd) +
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
             alpha = 0.7)
p_wt

ggsave(plot = p_wt,
       filename = "mean_daily_wt_all_sites_ET_P.tiff",
       device = "tiff",
       width = 8,
       height = 6,
       dpi = 300)


