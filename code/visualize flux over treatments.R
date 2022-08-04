
# PCA for showing how transplants and ambient plots group together.

# source clean data and metadata   --------------------------

source("code/data_dic/download_read_clean_data.R")
# source("code/metaturf.R")

# # combine all sites together  -------------------------------

# cflux_all <- bind_rows(cflux_vikesland, cflux_hogsete, cflux_joasete, cflux_liahovden)
# 
# cflux_all <- right_join(
#   cflux_all, metaturf)

## boxplot gpp   ---------------------------------------------------

p_gpp <- cflux %>% 
  mutate(origin = factor(origin, levels = c("vikesland", "hogsete", "joasete", "liahovden"))) %>%
  filter(type == "GPP") %>% 
  filter(PARavg >= 50) %>% 
  ggplot(aes(x = origin, y = flux, col= warming)) +
  # geom_boxplot() +
  # geom_jitter(shape = 16, col= "black", alpha = 3/10, position=position_jitter(0.2)) +
  # 
  geom_boxplot(outlier.shape=NA, lwd = 1) + 
  geom_point(position=position_jitterdodge(0.2), shape = 16, alpha = 3/10,)+

  # ggtitle("Differences in GPP between sites and treatments") +
  ylab("GPP (CO2 umol m2 h-1)")  +
  xlab("") +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank())
  # theme(strip.text.y = element_text(size = 14, colour = "black"),
  #       axis.ticks = element_line(size=1.5), 
  #       axis.text.x = element_text(angle = 20, vjust = 0.8, hjust=0.8),
  #       axis.title = element_text(size = 16, color ="darkgrey"),
  #       #axis.title.x = element_blank(),
  #       axis.line = element_line(color = "grey"),
  #       axis.text = element_text(size = 12),
  #       legend.position = "right",
  #       legend.text = element_text(size = 12),
  #       legend.title = element_blank(),
  #       # legend.title = element_text(size = 8),
  #       #legend.key.width = unit(0.4, 'cm'),
  #       #panel.grid.major.x = element_blank(), 
  #       #panel.grid.major.y = element_blank(),
  #       panel.grid.minor.x = element_blank(),
  #       panel.grid.minor.y = element_blank(),
  #       plot.title = element_text(size=16))
  # # panel.background = element_rect(
  # #  fill = 'white', colour = 'grey')) 

p_gpp <- p_gpp + scale_color_manual(values = c(
     "ambient" = "#1e90ff",
    "transplant" = "#ff0800"
  )) 


## boxplot gpp   ---------------------------------------------------

p_er <- cflux %>% 
  mutate(origin = factor(origin, levels = c("vikesland", "hogsete", "joasete", "liahovden"))) %>%
  filter(type =="ER") %>% 
  # filter(PARavg >= 50) %>% 
  ggplot(aes(x = origin, y = flux, col = warming)) +
  # geom_boxplot() +
  # geom_jitter(shape = 16, col= "black", alpha = 3/10, position=position_jitter(0.2)) +
  # 
  geom_boxplot(outlier.shape = NA, lwd = 1) + 
  geom_point(position=position_jitterdodge(0.2), shape = 16, alpha = 3/10)+
  
  # ggtitle("Differences in ER between sites and treatments") +
  ylab("ER (CO2 umol m2 h-1)") +
  xlab("") +
  theme_bw() +
  theme(legend.position = "bottom", 
       legend.title = element_blank())
  
#   theme(strip.text.y = element_text(size = 14, colour = "black"),
#         axis.ticks = element_line(size=1.5), 
#         axis.text.x = element_text(angle = 20, vjust = 0.8, hjust=0.8),
#         axis.title = element_text(size = 16, color ="darkgrey"),
#         #axis.title.x = element_blank(),
#         axis.line = element_line(color = "grey"),
#         axis.text = element_text(size = 12),
#         legend.position = "right",
#         legend.text = element_text(size = 12),
#         legend.title = element_blank(),
#         # legend.title = element_text(size = 8),
#         #legend.key.width = unit(0.4, 'cm'),
#         #panel.grid.major.x = element_blank(), 
#         #panel.grid.major.y = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         plot.title = element_text(size=16))
# # panel.background = element_rect(
# #  fill = 'white', colour = 'grey')) 

p_er <- p_er + scale_color_manual(values = c(
  "ambient" = "#1e90ff",
  "transplant" = "#ff0800"
)) 

p_gpp / p_er
