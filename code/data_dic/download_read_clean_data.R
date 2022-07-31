library(dataDownloader)
library(tidyverse)


# meta turfID -------------------------------------------------------------

get_file(node = "pk4bg",
         file = "Three-D_metaturfID.csv",
         path = "clean_data",
         remote_path = "Site")

metadata <- read_csv("data_clean/Three-D_metaturfID.csv")


