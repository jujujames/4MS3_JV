# prepare data-----


# load libraries
rm(list=ls())
gc(reset = TRUE)
install.packages('easypackages')
easypackages::packages('geobr', 'magick', 'gtfs2gps', 
                       'data.table', 'sf', 'mapview', 'progressr',
                       'magrittr', 'dplyr', 'ggnewscale',
                       'ggplot2', 'ggmap', 'raster', 'terra',
                       'rayshader', 'rayrender', 'rayimage')

# 1) Download Toronto's shapefile ------
install.packages("cancensus")
library(cancensus)

options(cancensus.api_key = "CensusMapper_08b53dd19db8e68621df4b7b19853e48")


toronto_bound <- cancensus::get_census(
  dataset = "CA21",   
  regions = list(CSD = "3520005"),  
  level = "CSD",      
  geo_format = "sf"
)

toronto_bound <- sf::st_transform(toronto_bound, 4326)  

readr::write_rds(toronto_bound, "data/toronto_bound_CSD.rds")

# 1) PREP DATA FOR SPATIAL PLOTS ------
#ttc_gtfs_url <- "https://ckan0.cf.opendata.inter.prod-toronto.ca/dataset/7795b45e-e65a-4465-81fc-c36b9dfff169/resource/cfb6b2b8-6191-41e3-bda1-b175c51148cb/download/TTC%20Routes%20and%20Schedules%20Data.zip"
#download.file(url = ttc_gtfs_url, destfile = "data-raw/ttc_gtfs.zip")

# 2) Adjust EMTU GTFS -----
ttc_gtfs_raw <- gtfstools::read_gtfs("data-raw/ttc_gtfs.zip")

# exporting temporal intersections ---------------------------


sp_gtfs_tmp <- gtfstools::filter_by_shape_id(sp_gtfs_raw,c("540298_ida","502070_ida"))
sp_gtfs_tmp <- gtfstools::filter_by_trip_id(sp_gtfs_tmp,c("502070_ida_2_6","540298_ida_0_36"))

gps_tmp <- gtfs2gps::gtfs2gps(sp_gtfs_tmp)
gps_tmp <- gtfs2gps::adjust_speed(gps_tmp)

readr::write_rds(x = gps_tmp, "data/emtu_intersection_gps.rds")