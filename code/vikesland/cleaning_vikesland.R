
# This script will be to separate c-flux data into turfIDs and clean the fluxes before we calculate them

source("code/functions.R")

library("dataDownloader")

# download raw data
# download files from OSF ---------------------------------------

get_file(node = "pk4bg",
         file = "Three-D_24h-cflux_vikesland_2022.csv",
         path = "raw_data",
         remote_path = "RawData/C-Flux")

get_file(node = "pk4bg",
         file = "PFTC6_cflux_field-record_vikesland.csv",
         path = "raw_data",
         remote_path = "RawData/C-Flux")

get_file(node = "pk4bg",
         file = "PFTC6_cflux_cutting_vikesland.csv",
         path = "raw_data",
         remote_path = "RawData/C-Flux")

# If you manage to download dataDownloader and download the data, you are good! Congrats!
# In case you did not manage to download the data manually. Call me :-)

# cleaning Vikesland ------------------------------------------------------
# read the files
co2_24h_vikesland <- read_csv("raw_data/Three-D_24h-cflux_vikesland_2022.csv", na = c("#N/A"))
  
record_vikesland <- read_csv("raw_data/PFTC6_cflux_field-record_vikesland.csv", na = c(""))

# matching the CO2 concentration data with the turfs using the field record
# we have defined a default window length of 60 secs.

co2_fluxes_vikesland_60 <- match.flux.PFTC6(co2_24h_vikesland, record_vikesland, window_length = 60)

# cutting Vikesland ------------------------------------------------------
cutting_vikesland <- read_csv("raw_data/PFTC6_cflux_cutting_vikesland.csv", na = "", col_types = "dcc")

co2_cut_vikesland_60 <- co2_fluxes_vikesland_60 %>% 
  left_join(cutting_vikesland, by = "fluxID") %>% 
  mutate(
    start_cut = ymd_hms(paste(date, .$start_cut)),
    end_cut = ymd_hms(paste(date, .$end_cut))
  )

# adjusting the time window with manual cuts ------------------------------------------------------

co2_cut_vikesland_60 <- co2_cut_vikesland_60 %>%
  mutate(
  start_window = case_when(
    is.na(start_cut) == FALSE ~ start_cut,
    TRUE ~ start_window
  ),
  end_window = case_when(
    is.na(end_cut) == FALSE ~ end_cut,
    TRUE ~ end_window
  ),
  cut = case_when(
    datetime <= start_window | datetime >= end_window ~ "cut",
    # fluxID ==  & datetime %in%  ~ "cut",
    TRUE ~ "keep"
    ),
  cut = as_factor(cut)
  )

# vizz Vikesland -------------------------------------------------------

# visualizing 60 secs cuts in Vikesland (it´s in comments, just in case you don´t want to visualize it)

# theme_set(theme_grey(base_size = 5))
# 
# co2_cut_vikesland_60 %>%
#   ggplot(aes(x = datetime, y = CO2, colour = cut)) +
#   geom_line(size = 0.2, aes(group = fluxID)) +
#   # geom_line(size = 0.2) +
#   scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
#   # scale_x_date(date_labels = "%H:%M:%S") +
#   facet_wrap(vars(fluxID), ncol = 30, scales = "free")

# ggsave("fluxes_details_vikesland.png", height = 40, width = 80, units = "cm")


# produce clean CO2 cut --------------------------------------------------------

co2_cut_60_keep <- filter(co2_cut_vikesland_60,
                  cut == "keep")  #to keep only the part we want to keep

# cleaning PAR ------------------------------------------

# for ER we look at the range of PAR to see if there are errors
filter(co2_cut_60_keep, type == "ER") %>% #faster than looking at the graph!
  summarise(
    rangePAR = range(PAR)
  )

# for NEE we look at the range of PAR to see if there are errors

filter(co2_cut_60_keep, type == "NEE") %>% #faster than looking at the graph!
  summarise(
    rangePAR = range(PAR)
  )

# visualize PAR level ---------------------------------
# ER ---------------------------------

filt_ER_60 <- filter(co2_cut_60_keep, type == "ER") # I am just filtering to make things easier
# quick plot
plot(filt_ER_60$PAR) # Plot the PAR values
plot(x= filt_ER_60$datetime, y= filt_ER_60$PAR) # Plot the PAR vs time
abline(h=0, col="red")

# now we are replacing negative PAR values in type=ER by zero values.

co2_cut_60_keep <- co2_cut_60_keep %>% 
  mutate(
    PAR =
  case_when(
    type=="ER" & PAR <= 0 ~ 0, 
    TRUE~PAR
    )
  )

# let´s plot the PAR values for ER again:

filt_ER_60 <- filter(co2_cut_60_keep, type == "ER")

plot(x= filt_ER_60$datetime, y= filt_ER_60$PAR) # Plot the PAR vs time
abline(h=0, col="red")

#unique(filt_ER_60[filt_ER_60$PAR > 60,]$fluxID) # identify the weird values 
#range(filt_ER_60[filt_ER_60$PAR > 60,]$PAR) # and the PAR levels (no big deal)
#unique(filt_ER_60[filt_ER_60$PAR > 60,]$datetime) # who was on the field at this time...

#  NEE ---------------------------------

filt_NEE_60 <- filter(co2_cut_60_keep, type == "NEE") # I am just filtering to make things easier

# quick plot
plot(filt_NEE_60$PAR) # Plot the PAR values
plot(x= filt_NEE_60$datetime, y= filt_NEE_60$PAR,
     xlab = "Time of the day (hours)", 
     ylab = "Photosynthetically active radiation (PAR)",
     col = alpha("blue", 0.1), pch=16,
) # Plot the PAR vs time

abline(h = 0, col="blue")

# now we are replacing negative and odd PAR values in type = NEE by NA values


theme_set(theme_grey(base_size = 5))

filt_NEE_60 %>%
  ggplot(aes(x = datetime, y = PAR, colour = cut)) +
  geom_point(size = 0.2, aes(group = fluxID)) +
  # geom_line(size = 0.2) +
  scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
  # scale_x_date(date_labels = "%H:%M:%S") +
  facet_wrap(vars(fluxID), ncol = 30, scales = "free")


co2_cut_60_keep <- co2_cut_60_keep %>% 
  mutate(
    PAR =
      case_when(
          fluxID == "195"
          ~ NA_real_, 
        TRUE~PAR
      )
  )


co2_cut_60_keep <- co2_cut_60_keep %>% 
  mutate(
    PAR =
      case_when(
        fluxID == "143"
        ~ NA_real_, 
        TRUE~PAR
      )
  )


co2_cut_60_keep <- co2_cut_60_keep %>% 
  mutate(
    PAR =
      case_when(
        fluxID == "153"
        ~ NA_real_, 
        TRUE~PAR
      )
  )

co2_cut_60_keep <- co2_cut_60_keep %>% 
  mutate(
    PAR =
      case_when(
        fluxID == "155"
        ~ NA_real_, 
        TRUE~PAR
      )
  )

co2_cut_60_keep <- co2_cut_60_keep %>% 
  mutate(
    PAR =
      case_when(
        fluxID == "177"
        ~ NA_real_, 
        TRUE~PAR
      )
  )

co2_cut_60_keep <- co2_cut_60_keep %>% 
  mutate(
    PAR =
      case_when(
        fluxID == "179"
        ~ NA_real_, 
        TRUE~PAR
      )
  )

co2_cut_60_keep <- co2_cut_60_keep %>% 
  mutate(
    PAR =
      case_when(
        fluxID == "161" & PAR < 50
        ~ NA_real_, 
        TRUE~PAR
      )
  )


filt_NEE_60 <- filter(co2_cut_60_keep, type == "NEE") # I am just filtering to make things easier

# quick plot
plot(filt_NEE_60$PAR) # Plot the PAR values
plot(x= filt_NEE_60$datetime, y= filt_NEE_60$PAR,
     xlab = "Time of the day (hours)", 
     ylab = "Photosynthetically active radiation (PAR)",
     col = alpha("blue", 0.1), pch=16,
) # Plot the PAR vs time

abline(h = 0, col="blue")

#### same plots with ggplot coding

co2_cut_60_keep %>% 
  filter(
    type == "ER"
  ) %>% 
  ggplot(aes(datetime, PAR)) +
  geom_point()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

co2_cut_60_keep %>% 
  filter(
    type == "NEE"
  ) %>% 
  ggplot(aes(datetime, PAR)) +
  geom_point()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

# for ER we look at the range of PAR to see if there are still errors
filter(co2_cut_60_keep, type == "ER") %>% #faster than looking at the graph!
  summarise(
    rangePAR = range(PAR)
  )

# ... what should we do now??
# 1. think about weird PAR values. what could be happening, and how to solve it? (Discuss in class)

# 2. we should also manually modify the cuts for those curve that does not look fine with the automatic cuts.



# calculation of fluxes ---------------------------------------------------

cflux_vikesland <- co2_cut_60_keep %>% 
  flux.calc.PFTC6()


# calculating GPP ---------------------------------------------------------

cflux_vikesland_GPP <- cflux_vikesland %>%
  mutate(
    pairID = case_when(
      type == "NEE" ~ fluxID,
      type == "ER" ~ fluxID-1
    ),   # problem with datetime, it is different between ER and NEE. Let's use datetime NEE
    datetime = case_when(
      type == "NEE" ~ datetime,
      type == "ER" ~ NA_real_
    ),
    turfID = as_factor(turfID),
    type = as_factor(type)
  ) %>% 
  select(!c(fluxID, temp_soilavg)) %>%
  # pivot_wider(names_from = type, values_from = PARavg, names_prefix = "PARavg_") %>% 
  # select(!c(PAR_corrected_flux)) %>%
  # select(campaign, turfID, date, type, corrected_flux) %>%
  pivot_wider(id_cols = datetime, names_from = type, values_from = flux)
  
  # pivot_wider(names_from = type, values_from = c(flux, temp_soilavg)) %>% 
  rename(
    ER = flux_ER,
    NEE = flux_NEE
  ) %>%
  mutate(
    GEP = NEE - ER
  ) %>% 
  pivot_longer(c(ER, NEE, GEP), names_to = "type", values_to = "flux") %>% 
  mutate(
    temp_soil = case_when(
      type == "ER" ~ temp_soilavg_ER,
      type == "NEE" ~ temp_soilavg_NEE,
      type == "GEP" ~ rowMeans(select(., c(temp_soilavg_NEE, temp_soilavg_ER)), na.rm = TRUE)
    )
  ) %>% 
  select(!c(temp_soilavg_ER, temp_soilavg_NEE))

write_csv(cflux_vikesland, "clean_data/Three-D_24h-cflux_vikesland_2022.csv")

