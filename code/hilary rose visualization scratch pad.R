library(tidyverse)
library(lubridate)
library(hms)
library(patchwork)

# Merge data from all sites ----

# This is still messy because something went wrong with the site
meta_seedclim <- tibble(
  turfID = c("TTC 101", "TTC 110", "TTC 115", "TTC 146", "TTC 140", "TTC 141"),
  destSiteID = c("Hog", "Hog", "Hog", "Vik", "Vik", "Vik"),
  origSiteID = c("Hog", "Hog", "Hog", "Vik", "Vik", "Vik"),
  warming = "A"
) %>%
  bind_rows(read_csv("raw_data/Three-D_metaturfID.csv")) %>%
  mutate(turfID = as.character(turfID)) 

cflux_all = cflux_vikesland %>%
  bind_rows(cflux_hogsete) %>%
  bind_rows(cflux_liahovden) %>%
  left_join(meta_seedclim) %>%
  mutate(datetime = ymd_hms(datetime),
    time = as_hms(datetime))

colnames(cflux_all)

# Scratch plot for GPP ~ PAR ----

ggplot(cflux_all %>% filter(type == "GPP"), aes(y = flux, x = PARavg, color = destSiteID, shape = warming)) +
  geom_point() +
  geom_smooth() +
  scale_x_log10() +
  theme_bw()

# Scratch plot for time ----
hog.plot.er = 
  ggplot(cflux_all %>% filter(origSiteID == "Hog") %>% filter(type == "ER"), 
       aes(y = flux, x = time, color = warming)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.3) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  scale_color_manual(values = c("dodgerblue4", "firebrick4")) +
  facet_grid(type ~.) +
  ylim(-70, 80) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  labs(title = "Ecosystem respiration (ER)", x = "Time") 

hog.plot.gpp = 
  ggplot(cflux_all %>% filter(origSiteID == "Hog") %>% filter(type == "GPP"), 
         aes(y = flux, x = time, color = warming)) +
  # geom_line(inherit.aes = FALSE, aes(x = time, y = log(PARavg)), 
  #           color = "goldenrod1") +
  # geom_area(inherit.aes = FALSE, aes(x = time, y = log(PARavg)), 
  #           fill = "goldenrod1", alpha = 0.5) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.3) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  scale_color_manual(values = c("dodgerblue4", "firebrick4")) +
  facet_grid(type ~.) +
  theme_bw() +
  ylim(-70, 80) +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank()
  )+
  labs(title = "Gross primary productivity (GPP)", x = "") 

hog.plot.par = 
  ggplot(cflux_all %>% filter(destSiteID == "Hog") %>% filter(type == "GPP"), 
         aes(y = PARavg, x = time)) +
  geom_smooth(method = "loess", span = 1/3, color = "goldenrod1", se = FALSE) +
  stat_smooth(
      geom = 'area', method = 'loess', span = 1/3,
      fill = "goldenrod1", alpha = 0.5) + 
  geom_point(color = "goldenrod2") +
  scale_color_manual(values = c("dodgerblue4", "firebrick4")) +
  facet_grid(type ~.) +
  theme_bw() +
  scale_y_log10() +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank()
  )+
  labs(title = "Reflectance (PAR)", x = "") 

hog.plot.gpp + hog.plot.par + hog.plot.er +
  plot_layout(ncol = 1, heights = c(2, 1, 2)) 

png("visualizations/flux_PAR_Hog.png", res = 300, units = "in", width = 10, height = 10)
dev.off()

# Comparing origin and destination plot viz ----
ggplot(cflux_all, aes(y = flux, x = time, color = origSiteID)) +
  geom_point() +
  geom_smooth(inherit.aes = FALSE, data = cflux_all %>% filter(warming == "W"),
              aes(y = flux, x = time, color = origSiteID)) +
  geom_smooth(inherit.aes = FALSE, data = cflux_all %>% filter(warming == "A"),
              aes(y = flux, x = time, color = origSiteID), linetype = "dashed") +
  facet_grid(type ~.) +
  theme_bw()

ggplot(cflux_all, aes(y = flux, x = time, color = destSiteID)) +
  geom_point() +
  geom_smooth() +
  facet_grid(type ~.) +
  theme_bw()
