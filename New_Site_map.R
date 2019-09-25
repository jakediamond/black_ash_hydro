# 
# Purpose: To plot site locations with some hydro meta data
# Coder: Jake Diamond
# Date: May 15, 2018
# 

# Set working directory
setwd("C:/Users/diamo/Dropbox/Projects/EAB/Data")

library(ggmap)
# Google now needs API...
# if(!requireNamespace("devtools")) install.packages("devtools")
# devtools::install_github("dkahle/ggmap", ref = "tidyup")
register_google(key = "AIzaSyDLCGET5bdTMniSAH8oGDmuz19cYrJFNGY")
library(ggrepel)
library(cowplot)
library(rgdal)
library(dplyr)

# Read in lat long
df <- read.csv("HydroData/MetaData/well_latlong.csv")

# Load hydro data
df_hydro <- read.csv("average_wt_info_new_sites_v4.csv")

# Load peat depth data, find average per site
df_peat <- read.csv("peat_depths.csv", stringsAsFactors = FALSE)
df_peat[df_peat$site == "B1", "site"] <- "L1"
df_peat[df_peat$site == "B3", "site"] <- "L2"
df_peat[df_peat$site == "B6", "site"] <- "L3"
df_peat <- df_peat %>%
  mutate(depth = ifelse(depth == ">120",
                        130,
                        depth),
         depth = as.numeric(depth)) %>%
  group_by(site) %>%
  summarize(peat = mean(depth, na.rm = T))

# Combine data
df <- left_join(df, df_peat) %>%
  left_join(df_hydro)

# Load mn data
mn <- map_data("county", "minnesota")

# Load NHD Flowlines
fs <- readOGR("Spatial_data/NHD_H_0701_Shape/Shape", "NHDFlowline")
fs@data$id <- rownames(fs@data)
f <- fortify(fs)
f <- left_join(f, fs@data, by = "id")

# Get main satellite map
plot_loc <- get_map(location = c(lon = -94.1, 
                                 lat = 47.6), 
                    zoom = 9, 
                    maptype = "satellite")

# Main plot with hydroperiod
p_hydro <- ggmap(plot_loc) +
  geom_point(data = df, 
               aes(x = long, 
                   y = lat),
             shape = 21,
             fill = "white",
             size = 2) + 
  geom_label_repel(data = df,
                   aes(x = long,
                       y = lat,
                       label = site,
                       fill = avghydro_rel),
                   segment.color = 'white',
                   segment.size = 0.5,
                   min.segment.length = 0,
                   force = 0.5,
                   nudge_x = 0.01,
                   size = 8) +
  geom_path(data = dplyr::filter(f, 
                                 GNIS_NAME == "Mississippi River",
                                 long < -94.343 | long > -94.07),
            aes(x = long, y = lat,
                group = group),
            colour = "blue",
            size = 1) +
  annotate(geom = "text", x = -94.169, y = 47.463,
           label = "Lake Winnibigoshish",
           color = "white", size = 5) +
  annotate(geom = "text", x = -93.97, y = 47.332,
           angle = -35,
           label = "Mississippi \nRiver",
           color = "white", size = 5) +
  xlab("") + ylab("") + 
  theme(legend.background = 
          element_rect(fill = alpha("gray90", 0.5),
                       size = 0.7,
                       linetype = "solid",
                       colour = "black"),
        legend.title = element_text(face = "bold", 
                                    size = 24),
        # legend.title.align = 0.5,
        legend.text = element_text(size = 24),
        legend.position = c(0.77, 0.4),
        legend.margin = margin(t=0.2, r=0.2, b=0.35, l=0.2, unit="cm"),
        legend.key.width=unit(4, "line"),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  scale_x_continuous(limits = c(-94.6, -93.58),
                     expand = c(0, 0),
                     breaks = c(-94.6, -94.1, -93.6)) +
  scale_y_continuous(limits = c(47.23, 47.88),
                     expand = c(0, 0),
                     breaks = seq(47.25, 47.75,
                                0.25)) +
  scale_fill_gradient(name = "Mean \nhydro-\nperiod (d)",
                    limits = c(90, 160),
                    breaks = c(90, 120, 150)) +
  scaleBar(lon = -93.92, lat = 47.25,
           distanceLon = 7.5, distanceLat = 1,
           distanceLegend = 2.2, dist.unit = "km",
           arrow.length = 3.5,
           arrow.distance = 3.5,
           arrow.North.size = 6,
           legend.size = 5,
           legend.colour = "white",
           arrow.colour = "white",
           n.colour = "white")

# Main plot with peat depth
p_peat <- ggmap(plot_loc) +
  geom_point(data = df, 
             aes(x = long, 
                 y = lat),
             shape = 21,
             fill = "white",
             size = 2) + 
  geom_label_repel(data = df,
                   aes(x = long,
                       y = lat,
                       label = site,
                       fill = peat),
                   segment.color = 'white',
                   segment.size = 0.5,
                   min.segment.length = 0,
                   force = 0.5,
                   nudge_x = 0.01,
                   size = 8) +
  geom_path(data = dplyr::filter(f, 
                                 GNIS_NAME == "Mississippi River",
                                 long < -94.343 | long > -94.07),
            aes(x = long, y = lat,
                group = group),
            colour = "blue",
            size = 1) +
  annotate(geom = "text", x = -94.169, y = 47.463,
           label = "Lake Winnibigoshish",
           color = "white", size = 5) +
  annotate(geom = "text", x = -93.97, y = 47.332,
           angle = -35,
           label = "Mississippi \nRiver",
           color = "white", size = 5) +
  xlab("") + ylab("") + 
  theme(legend.background = 
          element_rect(fill = alpha("gray90", 0.5),
                       size = 0.7,
                       linetype = "solid",
                       colour = "black"),
        legend.title = element_text(face = "bold", 
                                    size = 24),
        # legend.title.align = 0.5,
        legend.text = element_text(size = 24),
        legend.position = c(0.77, 0.4),
        legend.margin = margin(t=0.2, r=0.2, b=0.35, l=0.2, unit="cm"),
        legend.key.width=unit(4, "line"),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  scale_x_continuous(limits = c(-94.6, -93.58),
                     expand = c(0, 0),
                     breaks = c(-94.6, -94.1, -93.6)) +
  scale_y_continuous(limits = c(47.23, 47.88),
                     expand = c(0, 0),
                     breaks = seq(47.25, 47.75,
                                  0.25)) +
  scale_fill_gradientn(name = "Mean \npeat \ndepth (cm)",
                      limits = c(15, 130),
                      breaks = c(20, 60, 100),
                      colours = terrain.colors(10)) +
  scaleBar(lon = -93.92, lat = 47.25,
           distanceLon = 7.5, distanceLat = 1,
           distanceLegend = 2.2, dist.unit = "km",
           arrow.length = 3.5,
           arrow.distance = 3.5,
           arrow.North.size = 6,
           legend.size = 5,
           legend.colour = "white",
           arrow.colour = "white",
           n.colour = "white")

# Extent rectangle for inset map
extent <- data.frame(xmin = -94.6, xmax = -93.6, 
                     ymin = 47.2, ymax = 47.9)

#Inset
p2 <- ggplot() + 
  geom_polygon(data = mn, 
               aes(long, 
                   lat, 
                   group = group), 
               colour = "grey10", 
               fill = "#fff7bc") +
  coord_map("albers", lat0 = 40, lat1 = 50) + 
  theme_bw() + labs(x = NULL, y = NULL) +
  geom_path(data = dplyr::filter(f, 
                                 GNIS_NAME == "Mississippi River"),
            aes(x = long, y = lat,
                group = group),
            colour = "blue",
            size = 1) +
  geom_rect(data = extent, 
            aes(xmin = xmin, xmax = xmax, 
                ymin = ymin, ymax = ymax), 
            alpha = 0, colour = "red", 
            size = 2, linetype = 1) +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Full peat plot with inset
p_full_peat <- ggdraw() +
  draw_plot(p_peat) +
  draw_plot(p2, 
            x = 0.19,
            y = 0.65,
            width = 0.3,
            height = 0.3,
            scale = 1)

# Full plot with inset
p_full_hydro <- ggdraw() +
  draw_plot(p_hydro) +
  draw_plot(p2, 
            x = 0.19,
            y = 0.65,
            width = 0.3,
            height = 0.3,
            scale = 1)

# Save plot
ggsave(plot = p_full_hydro, 
       "New_Sites_Hydroperiod.tiff",
       device = "tiff",
       width = 16,
       height = 10,
       units = "in",
       dpi = 600)

ggsave(plot = p_full_peat, 
       "New_Sites_Peat.tiff",
       device = "tiff",
       width = 16,
       height = 10,
       units = "in",
       dpi = 600)
  