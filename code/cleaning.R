
# This script will be to separate c-flux data into turfIDs and clean the fluxes before we calculate them

# This is the OSF site where the rawdata are installed:
# Three-D https://osf.io/pk4bg/

# re install data downloader
install.packages("remotes")

remotes::install_github("nyuglobalties/osfr@fix/use-wb-asset-id")

#load libraries

library("dataDownloader")
library(tidyverse)
library(lubridate)
library(broom)
# library(fs)
# library(zoo)
# library(slider)



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


# functions ----------------------------

match.flux.PFTC6 <- function(raw_flux, field_record, window_length = 90, startcrop = 10, measurement_length = 210){
  
  raw_flux <- raw_flux %>% 
    rename( #rename the columns with easier names to handle in the code
      datetime = "Date/Time",
      temp_air = "Temp_air ('C)",
      temp_soil = "Temp_soil ('C)",
      CO2 = "CO2 (ppm)",
      PAR = "PAR (umolsm2)"
    ) %>% 
    mutate(
      datetime = dmy_hms(datetime) #transformt the date into R date format
    ) %>% 
    select(datetime, temp_soil, temp_air, CO2, PAR)
  
  field_record <- field_record %>%
    mutate(
      starting_time = gsub("(\\d{2})(?=\\d{2})", "\\1:", starting_time, perl = TRUE), # to add the : in the time
      date = dmy(date), #date in R format
      start = ymd_hms(paste(date, starting_time)), #pasting date and time together to make datetime
      end = start + measurement_length, #creating column End
      start_window = start + startcrop, #cropping the start
      end_window = start_window + window_length, #cropping the end of the measurement
      fluxID = row_number() #adding an individual ID ot each flux, useful to join data or graph the fluxes
    ) %>% 
    select(start, end, start_window, end_window, fluxID, turfID, type, date)
  
  
  co2conc <- full_join(raw_flux, field_record, by = c("datetime" = "start"), keep = TRUE) %>% #joining both dataset in one
    fill(PAR,temp_air, temp_soil, turfID,type,start,end,start_window, end_window, fluxID, date) %>% #filling all rows with data from above
    
    filter(
      datetime <= end
      & datetime >= start) %>% #cropping the part of the flux that is after the End and before the Start
    
    return(co2conc)
}


flux.calc.PFTC6 <- function(co2conc, # dataset of CO2 concentration versus time (output of match.flux)
                       chamber_volume = 24.5, # volume of the flux chamber in L, default for Three-D chamber (25x24.5x40cm)
                       tube_volume = 0.075, # volume of the tubing in L, default for summer 2020 setup
                       atm_pressure = 1, # atmoshperic pressure, assumed 1 atm
                       plot_area = 0.0625 # area of the plot in m^2, default for Three-D
)
{
  R = 0.082057 #gas constant, in L*atm*K^(-1)*mol^(-1)
  vol = chamber_volume + tube_volume
  slopes <- co2conc %>% 
    group_by(fluxID) %>% #grouping CO2 concentration of each fluxes
    mutate(
      time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs") #the nb of seconds from the beginning of the measurement
    ) %>% 
    select(fluxID, time, CO2) %>%
    do({model = lm(CO2 ~ time, data=.)    # create your model
    data.frame(tidy(model),              # get coefficient info
               glance(model))}) %>%          # get model info
    filter(term == "time") %>% 
    rename(slope = estimate) %>%  #the slope is what we need to calculate the flux
    select(fluxID, slope, p.value, r.squared, adj.r.squared, nobs) %>% 
    ungroup()
  
  means <- co2conc %>% 
    group_by(fluxID) %>% 
    summarise(
      PARavg = mean(PAR, na.rm = TRUE), #mean value of PAR for each flux
      temp_airavg = mean(temp_air, na.rm = TRUE)  #mean value of temp_air for each flux
      + 273.15, #transforming in kelvin for calculation
      temp_soilavg = mean(temp_soil, na.rm = TRUE) #mean value of temp_soil for each flux
    ) %>% 
    ungroup()
  
  fluxes_final <- left_join(slopes, means, by = "fluxID") %>% 
    left_join(
      co2conc,
      by = "fluxID"
    ) %>% 
    select(fluxID, slope, p.value, r.squared, adj.r.squared, nobs, PARavg, temp_airavg, temp_soilavg, turfID, type, start_window) %>% 
    distinct() %>% 
    rename(
      datetime = start_window
    ) %>% 
    mutate(
      flux = (slope * atm_pressure * vol)/(R * temp_airavg * plot_area) #gives flux in micromol/s/m^2
      *3600 #secs to hours
      /1000 #micromol to mmol
    ) %>% #flux is now in mmol/m^2/h, which is more common
    arrange(datetime) %>% 
    select(!slope)
  
  return(fluxes_final)
  
}



# define timing -----------------------------------------------------------

# measurement <- 210 #the length of the measurement taken on the field in seconds (we make it longer to make sure to also see what happened at the end of the flux)
# startcrop <- 10 #how much to crop at the beginning of the measurement in seconds
# endcrop <- 40 #how much to crop at the end of the measurement in seconds


# cleaning Vikesland ------------------------------------------------------

cflux_24h_vikesland <- read_csv("raw_data/Three-D_24h-cflux_vikesland_2022.csv", na = c("#N/A"))
  
record_vikesland <- read_csv("raw_data/PFTC6_cflux_field-record_vikesland.csv", na = c(""))

# matching the CO2 concentration data with the turfs using the field record
# we have defined three window lenghts 90, 60 and 40 secs.
# so, we create three files:

co2_fluxes_vikesland_90 <- match.flux.PFTC6(cflux_24h_vikesland, record_vikesland)

co2_fluxes_vikesland_60 <- match.flux.PFTC6(cflux_24h_vikesland, record_vikesland, window_length = 60)

co2_fluxes_vikesland_40 <- match.flux.PFTC6(cflux_24h_vikesland, record_vikesland, window_length = 40)

# cutting
# we now do three cuts, with the previous files; so we are going to end up with three files again (one per window length)
cutting_vikesland <- read_csv("raw_data/PFTC6_cflux_cutting_vikesland.csv", na = "", col_types = "dtt")

co2_cut_vikesland_90 <- co2_fluxes_vikesland_90 %>% 
  left_join(cutting_vikesland, by = "fluxID") %>% 
  mutate(
    start_cut = ymd_hms(paste(date, .$start_cut)),
    end_cut = ymd_hms(paste(date, .$end_cut))
  )

co2_cut_vikesland_60 <- co2_fluxes_vikesland_60 %>% 
  left_join(cutting_vikesland, by = "fluxID") %>% 
  mutate(
    start_cut = ymd_hms(paste(date, .$start_cut)),
    end_cut = ymd_hms(paste(date, .$end_cut))
  )

co2_cut_vikesland_40 <- co2_fluxes_vikesland_40 %>% 
  left_join(cutting_vikesland, by = "fluxID") %>% 
  mutate(
    start_cut = ymd_hms(paste(date, .$start_cut)),
    end_cut = ymd_hms(paste(date, .$end_cut))
  )


# adjusting the time window with manual cuts
# again three times (90,60,40)
co2_cut_vikesland_90 <- co2_cut_vikesland_90 %>%
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

co2_cut_vikesland_40 <- co2_cut_vikesland_40 %>%
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

# we can now graph independently each of the three graphs...
# we are now visualizing only the 90 one, but you can change the one to display manually

theme_set(theme_grey(base_size = 5)) 

co2_cut_vikesland_90 %>% 
  ggplot(aes(x = datetime, y = CO2, colour = cut)) +
  geom_line(size = 0.2, aes(group = fluxID)) +
  # geom_line(size = 0.2) +
  scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
  # scale_x_date(date_labels = "%H:%M:%S") +
  facet_wrap(vars(fluxID), ncol = 30, scales = "free")
  
# ggsave("fluxes_details_vikesland.png", height = 40, width = 80, units = "cm")

co2_cut_90_keep <- filter(co2_cut_vikesland_90,
                  cut == "keep")  #to keep only the part we want to keep
                    

# cleaning PAR --------------------------------------------------------------
# will need to graph the NEE to check for negative and weird values
  

# for ER we look at the range of PAR to see if there are errors
   filter(co2_cut_90_keep, type == "ER") %>% #faster than looking at the graph!
     summarise(
     rangePAR = range(PAR)
     )

# visualize PAR levels

filt_ER_90 <- filter(co2_cut_90_keep, type == "ER") # I am just filtering to make things easier
plot(filt_ER_90$PAR) # Plot the PAR
unique(filt_ER_90[filt_ER_90$PAR>60,]$fluxID) # identify the weird values 
range(filt_ER_90[filt_ER_90$PAR>60,]$PAR) # and the PAR levels (no big deal)
unique(filt_ER_90[filt_ER_90$PAR>60,]$datetime) # who was on the field at this time...

## Summarizing we had some problems in plots 221,223,225 and 227 around 17:00 but the CO2 levels look good, so no big deal 


# clean soil temp ---------------------------------------------------------
# will need to graph

# in case we forgot to put the temp sensor in the ground 
 #  co2_cut_vikesland <- co2_cut_90_keep %>% 
  #   mutate(
  #     temp_soil = case_when(
  #       comments == "soilT logger not plugged in" ~ NA_real_,
   #      comments == "Soil T NA" ~ NA_real_,
   #      TRUE ~ temp_soil
   #    )
   #  )


# clean air temperature ---------------------------------------------------

# will need to make a graph

# calculate the fluxes ----------------------------------------------------
# 
#   fluxes_vikesland <- flux.calc.PFTC6(co2_cut_vikesland) %>% 
#     mutate(
#       PARavg = case_when(
#         is.nan(PARavg) == TRUE ~ NA_real_, #mean(PAR) returned NaN when PAR was all NAs but it is missing values
#         TRUE ~ as.numeric(PARavg)
#       )
#     )

