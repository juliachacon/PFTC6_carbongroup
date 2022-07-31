

# Fluxes calculations
# FUNCTION

flux.calc2 <- function(co2conc, # dataset of CO2 concentration versus time (output of match.flux)
            chamber_volume = 24.5, # volume of the flux chamber in L, default for Three-D chamber (25x24.5x40cm)
            tube_volume = 0.075, # volume of the tubing in L, default for summer 2020 setup
            atm_pressure = 1, # atmoshperic pressure, assumed 1 atm
            plot_area = 0.0625 # area of the plot in m^2, default for Three-D
 )
 {
   R = 0.082057 #gas constant, in L*atm*K^(-1)*mol^(-1)
   vol = chamber_volume + tube_volume
   # co2conc <- co2_cut
   slopes <- co2conc %>% 
     group_by(fluxID) %>% 
     mutate(
       time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs")
     ) %>% 
     select(fluxID, time, CO2) %>%
     do({model = lm(CO2 ~ time, data=.)    # create your model
     data.frame(tidy(model),              # get coefficient info
                glance(model))}) %>%          # get model info
     filter(term == "time") %>% 
     rename(slope = estimate) %>% 
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
# 

### Calc fluxes for 90,60 and 40 secs.
flux.virkesland.90 <- flux.calc2(co2_cut_90_keep) 
# flux.virkesland.60 <- flux.calc2(co2_cut_60_keep) 
# flux.virkesland.40 <- flux.calc2(co2_cut_40_keep) 


### Fluxes quality (check later, we need to see...)
# fluxes_quality <- flux.virkesland.90 %>% mutate(
#   significance = case_when(
#     p.value <= 0.05 ~ "significant",
#     p.value > 0.05 ~ "non-significant"
# #   ),
#   significance = as.factor(significance),
#   quality = case_when(
#     adj.r.squared >= 0.6 ~ "good",
#     adj.r.squared < 0.6 ~ "bad"
#   ),
#   quality = as.factor(quality)
# ) %>% 
#   select(turfID, quality, significance) %>% 
#   right_join(co2_cut) %>% 
#   group_by(fluxID) %>% 
#   mutate(
#     time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs")
#   ) %>% 
#   ungroup()
