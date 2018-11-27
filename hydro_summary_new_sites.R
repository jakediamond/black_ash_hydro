#
# Purpose: To compare water table across sites
# Coder: Jake Diamond
# Date: November 21, 2017
#

# Set Working directory
# setwd("E:/Dropbox/Dropbox/Projects/EAB/Data/HydroData")
setwd("C:/Users/diamo/Dropbox/Projects/EAB/Data/HydroData")

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(cowplot)
library(egg)

# Read in water table data
df <- read.csv("alldata_new_sites.csv")

# Get data ready, clean, format times and dates
df$datetime <- as.POSIXct(df$datetime, 
                          format = "%Y-%m-%d %H:%M", 
                          tz = "UTC")
df$date <- as.Date(df$date, 
                   format = "%Y-%m-%d", 
                   tz = "UTC")
df$X <- NULL

# For later plot, make everything in the same year (2015) 
df$date2015 <- as.Date(format(df$date, format = "%d-%m-2015"), 
                   format = "%d-%m-%Y",
                   tz = "UTC")
df$datetime2015 = as.POSIXct(format(df$datetime, 
                                 format = "%d-%m-2015 %H:%M"), 
                      format = "%d-%m-%Y %H:%M",
                      tz = "UTC")

# Check manual water table validation data
df_val <- read.csv("MetaData/New Sites/wt_validation.csv")

df_val$datetime <- as.POSIXct(df_val$datetime_use, 
                                  format = "%m/%d/%Y %H:%M", 
                                  tz = "UTC")

df_val$datetime2015 <- as.POSIXct(format(df_val$datetime, 
                                         format = "%d-%m-2015 %H:%M"), 
                                  format = "%d-%m-%Y %H:%M")

val_compare <- left_join(df_val, df,
                         by = c("site", "year", "datetime")) %>%
  group_by(site, datetime) %>%
  summarize(wl_obs = wl_obs,
            waterlevel = waterlevel,
            dif = wl_obs/ 100 - waterlevel)

# Remove data that is below sensor, or very close to sensor limit (except for D4 in 2017)
df[site == "D4" &
     year == 2017 &
     wtpress < 100, "waterlevel"] <- NA

df[!(site == "D4" &
       year == 2017) &
     wtpress < 250, "waterlevel"] <- NA

# Also remove known outliers/bad data
# df[df$site == "D4" & 
#      df$datetime == as.POSIXct("2017-09-30 15:15:00", tz = "UTC"),
#    "waterlevel"] <- NA

df[df$site == "D4" & 
     df$datetime == as.POSIXct("2018-10-21 16:45:00", tz = "UTC"),
   "waterlevel"] <- NA

df[df$site == "D1" & 
     df$datetime == as.POSIXct("2018-10-21 17:00:00", tz = "UTC"),
   "waterlevel"] <- NA

# Calculate daily means and rain sums
mean_wt_day <- df %>% 
  group_by(site, year, date2015) %>% 
  dplyr::summarize(meanwt = mean(waterlevel, na.rm = TRUE),
                   sumrain = sum(rain_15min, na.rm = TRUE))

# Plots of raw watertable and rainfall data
# First, get datetime limits for plots
limsdt <- as.POSIXct(strptime(c("2015-05-25 0:00",
                              "2015-11-06 0:00"), 
                            format = "%Y-%m-%d %H:%M"))
limsd <- as.Date(c("2015-05-25",
                   "2015-11-06"), 
                 format = "%Y-%m-%d")

# Color palette
colpal <- c("#66CCFF", "#0000FF", "#333399")

# Loop through each site to plot all on years on top of eachother
sites <- unique(df$site)

for(i in levels(sites)){
# Main water table Plot
p_wt <- ggplot(data = dplyr::filter(df,
                                    site == i),
               aes(x = datetime2015,
                   y = waterlevel * 100,
                   colour = factor(year),
                   group = year)) +
  geom_line(size = 1.5) +
  geom_point(data = dplyr::filter(df_val, site == i),
             aes(x = datetime2015,
                 y = wl_obs,
                 colour = factor(year),
                 group = year),
             shape = 1,
             size = 4) +
  scale_x_datetime(date_breaks = "2 weeks",
                   date_labels = "%b-%d",
                   limits = limsdt) +
  theme_bw() +
  scale_colour_manual(name = "Year",
                      values = colpal) +
  theme(axis.title.x = element_blank(),
        legend.position = c(0.92, 0.17),
        panel.grid = element_blank(),
        plot.margin = margin(0, 5.5, 5.5, 5.5)
        ) +
  ylab("Relative water table (cm)") +
  geom_hline(yintercept = 0, 
             linetype = "dashed",
             size = 1,
             color = "grey30",
             alpha = 0.7)

# Inset plot for diel
p_wt_inset <- ggplot(data = dplyr::filter(df,
                                          site == i,
                                          date < "2017-07-21",
                                          date > "2017-07-15"),
                     aes(x = datetime,
                         y = waterlevel * 100)) +
  geom_line(size = 1.5,
            colour = colpal[2]) +
  scale_x_datetime(date_breaks = "1 days",
               date_labels = "%H") +
  # scale_y_continuous(limits = c(-0.35, -0.2)) +
  theme_bw() +
  ylab("") + 
  ggtitle("July 15-21, 2017") +
  theme(axis.title.x = element_blank(),
        panel.grid.major.x = element_line(size = 6),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_blank())

# Plot rainfall
p_rain <- ggplot(data = dplyr::filter(mean_wt_day,
                                      site == i),
                 aes(x = date2015,
                     y = sumrain * 100,
                     fill = factor(year),
                     group = year)) +
  geom_col(position = "dodge") +
  scale_fill_manual(name = "Year",
                      values = colpal) +
  scale_y_reverse(position = "right") +
  scale_x_date(date_breaks = "2 weeks",
                   date_labels = "%b-%d",
                   limits = limsd) +
  theme_bw() +
  ylab("Daily rainfall (cm)") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y.right = element_text(vjust = 1),
        panel.grid = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(5.5, 5.5, 0, 5.5), "pt")
  )

p_all <- egg::ggarrange(p_rain, 
                    p_wt, 
                    ncol = 1, nrow = 2,
                    heights = c(0.5, 1))

# Draw full plot with inset
p_full <- ggdraw() +
  draw_plot(p_all) +
  draw_plot(p_wt_inset, 
            x = 0.07,
            y = 0.05,
            width = 0.25,
            height = 0.25)

# Save plot
ggsave(plot = p_full,
       filename = paste0(i,"_all_compareyears.tiff"),
       device = "tiff",
       width = 8,
       height = 6,
       dpi = 300)
}

# Hydrologic analysis
# Calculate means, medians, 80%/20% quantile
results <- mean_wt_day %>%
  group_by(site) %>%
  dplyr::filter(days < as.Date("2015-10-11"),
                days > as.Date("2015-05-25")) %>%
  dplyr::summarize(mean = mean(meanwt, 
                               na.rm = TRUE),
                   median = median(meanwt, 
                                   na.rm = TRUE),
                   quant_80 = quantile(meanwt, probs = 0.8, 
                                       na.rm = TRUE),
                   quant_20 = quantile(meanwt, probs = 0.2, 
                                       na.rm = TRUE),
                   variation = var(meanwt, 
                                   na.rm = TRUE)) %>%
  mutate(quant_ratio = quant_80 / quant_20)

# Calculate hydroperiod
hydroperiod <- mean_wt_day %>%
  dplyr::filter(days < as.Date("2015-10-11"),
                days > as.Date("2015-05-25")) %>%
  group_by(site, year) %>%
  dplyr::summarize(hydroperiod = sum(meanwt > 0, na.rm = TRUE)) %>%
  group_by(site) %>%
  dplyr::summarize(avghydro = mean(hydroperiod))

results <- left_join(results, hydroperiod)

results.l <- results %>%
  gather(key = "metric", value = "value", -site)

ggplot(data = results.l,
       aes(x = site,
           y = value)) + 
  geom_col() + 
  theme_bw() + 
  facet_wrap(~metric, scales = "free")


write.csv(results_sub, "average_wt_info_new_sites.csv")

