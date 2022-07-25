
# This script will be to separate c-flux data into turfIDs and clean the fluxes before we calculate them

# This is the OSF site where the rawdata are installed:
# Three-D https://osf.io/pk4bg/

# re install data downloader
remotes::install_github("nyuglobalties/osfr@fix/use-wb-asset-id")

library("dataDownloader")

#download and unzip files from OSF
get_file(node = "pk4bg",
         file = "Three-D_24h-cflux_vikesland_2022.csv",
         path = "raw_data",
         remote_path = "RawData/C-Flux")

get_file(node = "pk4bg",
         file = "PFTC6_cflux_field-record_vikesland.csv",
         path = "raw_data",
         remote_path = "RawData/C-Flux")

# If you manage to download dataDownloader and download the data, you are good! Congrats!
# In case you did not manage to download the data manually. Call me :-)