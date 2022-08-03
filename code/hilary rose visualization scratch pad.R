library(tidyverse)
library(lubridate)

# Merge data from all sites ----

# This is still messy because something went wrong with the site
meta_seedclim <- tibble(
  turfID = c("TTC 101", "TTC 110", "TTC 115", "TTC 146", "TTC 140", "TTC 141"),
  destSiteID = c("Hog", "Hog", "Hog", "Vik", "Vik", "Vik"),
  origSiteID = c("Hog", "Hog", "Hog", "Vik", "Vik", "Vik"),
  warming = "A"
) %>%
  bind_rows(read_csv("raw_data/Three-D_metaturfID.csv")) 

cflux_all = cflux_vikesland %>%
  bind_rows(cflux_hogsete) %>%
  bind_rows(cflux_liahovden) %>%
  left_join(meta_seedclim) %>%
  mutate(datetime = ymd_hms(datetime),
    # hour = hour(datetime),
    # minute = minute(datetime),
    # second = second(datetime),
    # time = paste(hour, minute, second, sep = ":"),
    time = as_hms(datetime))

colnames(cflux_all)

# Scratch plot for GPP ~ PAR ----

ggplot(cflux_all %>% filter(type == "GPP"), aes(y = flux, x = PARavg, color = destSiteID, shape = warming)) +
  geom_point() +
  geom_smooth() +
  scale_x_log10() +
  theme_bw()

# Scratch plot for time ----
ggplot(cflux_all %>% filter(destSiteID == "Vik"), 
       aes(y = flux, x = datetime, color = warming)) +
  geom_line(inherit.aes = FALSE, aes(x = datetime, y = log(PARavg)), 
            color = "goldenrod1") +
  geom_area(inherit.aes = FALSE, aes(x = datetime, y = log(PARavg)), 
            fill = "goldenrod1", alpha = 0.5) +
  geom_point() +
  geom_smooth() +
  scale_color_manual(values = c("dodgerblue4", "firebrick4")) +
  facet_grid(type ~.) +
  theme_bw()

png("visualizations/flux_PAR_Vik.png", res = 300, units = "in", width = 10, height = 8)
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
