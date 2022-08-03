library(tidyverse)
library(lubridate)
library(hms)
library(patchwork)

# Merge data from all sites ----

# The metadata is still messy because something went wrong with the site
meta_seedclim <- tibble(
  turfID = c("TTC 101", "TTC 110", "TTC 115", "TTC 146", "TTC 140", "TTC 141"),
  destSiteID = c("Hog", "Hog", "Hog", "Vik", "Vik", "Vik"),
  origSiteID = c("Hog", "Hog", "Hog", "Vik", "Vik", "Vik"),
  warming = "A"
) %>%
  bind_rows(read_csv("raw_data/Three-D_metaturfID.csv")) %>%
  mutate(turfID = as.character(turfID)) 

# Join all the data together ----
cflux_all = cflux_vikesland %>%
  bind_rows(cflux_hogsete) %>%
  bind_rows(cflux_liahovden) %>%
  left_join(meta_seedclim) %>%
  mutate(datetime = ymd_hms(datetime),
         time = as_hms(datetime))

colnames(cflux_all)

# Scratch plot for time ----
plot.flux.time = function(orig.site, flux.type, ylim1, ylim2, title) {
  
  ggplot(cflux_all %>% filter(origSiteID == orig.site) %>% filter(type == flux.type), 
         aes(y = flux, x = time, color = warming)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.3) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  scale_color_manual(values = c("dodgerblue4", "firebrick4")) +
  facet_grid(type ~.) +
  ylim(ylim1, ylim2) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  labs(title = title, x = "Time") 
}

plot.par.time = function(dest.site) {
  ggplot(cflux_all %>% filter(destSiteID == dest.site) %>% filter(type == "GPP"), 
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
}

# Make the plots ----
hog.plot.er = plot.flux.time("Hog", "ER", -70, 80, "Ecosystem respiration (ER)")
hog.plot.gpp = plot.flux.time("Hog", "GPP", -70, 80, "Gross primary productivity (GPP)")
hog.plot.par = plot.par.time("Hog")


hog.plot.gpp + hog.plot.par + hog.plot.er +
  plot_layout(ncol = 1, heights = c(2, 1, 2)) 

png("visualizations/flux_PAR_Hog.png", res = 300, units = "in", width = 10, height = 10)
dev.off()
