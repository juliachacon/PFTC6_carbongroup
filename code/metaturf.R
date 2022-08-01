library(tidyverse)
library(dataDownloader)

get_file(node = "pk4bg",
         file = "Three-D_metaturfID.csv",
         path = "raw_data",
         remote_path = "Site")

metaturf <- read_csv("raw_data/Three-D_metaturfID.csv") %>% 
  select(warming, origSiteID, turfID) %>% 
  rename(
    Site = origSiteID,
    Warming = warming
  ) %>% 
  mutate(
    Site = str_replace_all(
      Site,
      c("Lia" = "Liahovden" , "Joa" = "Joasete", "Vik" = "Vikesland")
    ),
    Warming = str_replace_all(
      Warming,
      c("W" = "Transplant", "A" = "Ambient")
    )
  )
