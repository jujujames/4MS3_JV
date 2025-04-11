# prepare data
# load libraries

rm(list=ls())
gc(reset = TRUE)
install.packages('easypackages')
easypackages::packages('geobr', 'magick', 'gtfs2gps', 
                       'data.table', 'sf', 'mapview', 'progressr',
                       'magrittr', 'dplyr', 'ggnewscale',
                       'ggplot2', 'ggmap', 'raster', 'terra',
                       'rayshader', 'rayrender', 'rayimage', 'cancensus')

# 1) Retrieve Toronto's Census Subdivision Boundary ------

# Visit https://censusmapper.ca/api to obtain an API key for the cancensus package
# options(cancensus.api_key = "api key")


toronto_bound <- cancensus::get_census(
  dataset = "CA21",   
  regions = list(CSD = "3520005"),  
  level = "CSD",      
  geo_format = "sf",
  use_cache = TRUE
)

toronto_bound <- sf::st_transform(toronto_bound, 4326)  

readr::write_rds(toronto_bound, "data/toronto_bound_CSD.rds")

# 2) Retrieve GTFS DATA FOR SPATIAL PLOTS ------

# ttc_gtfs_url <- "https://ckan0.cf.opendata.inter.prod-toronto.ca/dataset/7795b45e-e65a-4465-81fc-c36b9dfff169/resource/cfb6b2b8-6191-41e3-bda1-b175c51148cb/download/TTC%20Routes%20and%20Schedules%20Data.zip"
# download.file(url = ttc_gtfs_url, destfile = "data-raw/ttc_gtfs.zip")

# 3) Adjust TTC GTFS -----

ttc_gtfs_raw <- gtfstools::read_gtfs("data-raw/ttc_gtfs.zip")

# add shape_id info on stop_times
ttc_gtfs_raw$stop_times[ttc_gtfs_raw$trips,on = "trip_id",shape_id := i.shape_id]

# pick a specific shape_id (bus route) for scripts 1.2.1 & 1.2.2
ttc_gtfs <- gtfstools::filter_by_shape_id(ttc_gtfs_raw,"1048831")

gtfs2gps::write_gtfs(ttc_gtfs,"data/gtfs_ttc_1048831.zip")

#Finding intersecting trips for script 1.1...I did this by manually sifting through the GTFS files
ttc_gtfs_tmp <- gtfstools::filter_by_shape_id(ttc_gtfs_raw,c("1048833","1049212"))
ttc_gtfs_tmp <- gtfstools::filter_by_trip_id(ttc_gtfs_tmp,c("48337409","48334845"))

gps_tmp <- gtfs2gps::gtfs2gps(ttc_gtfs_tmp)
gps_tmp <- gtfs2gps::adjust_speed(gps_tmp)

readr::write_rds(x = gps_tmp, "data/ttc_intersection_gps.rds")