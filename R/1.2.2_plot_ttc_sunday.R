# Load ----

rm(list=ls())
gc(reset = TRUE)
easypackages::packages('geobr'
                       , 'magick'
                       , 'gtfs2gps'
                       , 'data.table'
                       , 'sf'
                       , 'mapview'
                       , 'magrittr'
                       , 'dplyr'
                       , 'ggnewscale'
                       , 'ggplot2'
                       , 'rayshader'
                       , 'rayrender'
                       , 'rayimage'
                       , 'ggmap'
                       , 'raster'
                       , 'magick')


#  READ gps ----

ttc_gtfs <- gtfstools::read_gtfs("data/gtfs_ttc_1048831.zip")

# sunday
tmp_gtfs <- gtfstools::filter_by_weekday( ttc_gtfs,"sunday")
ttc_gps <- gtfs2gps::gtfs2gps(tmp_gtfs)

# define time window
time_start = "05:00:00"
time_end = "10:00:00"


# check start / end time
gps_dt <- rbind(ttc_gps[,day := "sunday"]) %>% 
  .[!is.na(timestamp)] %>% 
  .[,timestamp := data.table::as.ITime(timestamp)] %>% 
  .[,timestart := timestamp[1],by = .(trip_number,day)] %>% 
  .[,timeend := timestamp[.N],by = .(trip_number,day)] %>% 
  .[timestart >= as.ITime(time_start) & timeend <= as.ITime(time_end),] %>% 
  .[timeend > timestart,]

gps_dt[,.N,by=day]

# check GPS connections
tmp_gps <- data.table::copy(gps_dt) %>%
  .[,time := as.numeric(timestamp)]  %>%
  .[,shape_pt_lon_end := data.table::shift(shape_pt_lon,-1,NA), by = shape_id] %>%
  .[,shape_pt_lat_end := data.table::shift(shape_pt_lat,-1,NA), by = shape_id]

# create stops
tmp_stops <- data.table::copy(tmp_gps) %>%
  .[!is.na(cumtime) & !is.na(stop_id),] %>%  
  .[,time := as.numeric(timestamp)]  %>%
  .[,altitude := 100 * time/max(time)] %>% 
  data.table::setnames(.,old = c("shape_pt_lon","shape_pt_lat")
                       , new = c("X","Y"))

# View stops
view_tmp_stops <- data.table::copy(tmp_stops) %>% 
  sfheaders::sf_multipoint(.,x = "X"
                           ,  y = "Y"
                           , multipoint_id = "shape_id") %>%
  sf::st_set_crs(4326) %>%
  sf::st_transform(32617) %>%
  mapview::mapview()


# Create line
tmp_line <- data.table::copy(tmp_gps) %>%
  .[!is.na(cumtime) & !is.na(stop_id) & !is.na(timestamp),] %>%
  sfheaders::sf_linestring(obj = .
                           , x = "shape_pt_lon"
                           , y = "shape_pt_lat"
                           , linestring_id = "shape_id"
                           , keep = TRUE) %>%
  sf::st_set_crs(4326)

# bounding box
tmp_gps_bbox <- tmp_line %>%
  sf::st_transform(4326) %>%
  sf::st_transform(32617) %>%
  sf::st_buffer(x = .,dist = 8000) %>%
  sf::st_transform(4326) %>%
  sf::st_bbox() %>%
  as.numeric() %>%
  data.frame("X" = c(.[1],.[1],.[3],.[3])
             ,"Y" = c(.[2],.[4],.[4],.[2])) %>%
  sfheaders::sf_polygon(.,x = "X",y = "Y") %>%
  sf::st_set_crs(4326)

# download TILE ------

osm_bbox = tmp_gps_bbox %>% 
  raster::extent() %>%
  as.vector() %>% 
  .[c(1,3,2,4)]

view_osm_bbox <- sf::st_bbox(tmp_gps_bbox) %>% 
  mapview::mapview()

view_osm_bbox
# read tile -----
ggmap::file_drawer()
dir(file_drawer())
base_map <- ggmap::get_stadiamap(bbox = c(left = osm_bbox[1],
                                          bottom = osm_bbox[2],
                                          right = osm_bbox[3],
                                          top = osm_bbox[4]),
                                 maptype = "stamen_terrain",
                                 crop = TRUE,
                                 zoom = 12)

ggmap::file_drawer()


view_osm_bbox+view_tmp_stops

# produce gg base_map ----

my_plot_trans <- matrix(adjustcolor(base_map,
                                    alpha.f = 0.01),
                        nrow = nrow(base_map))
attributes(my_plot_trans) <-  attributes(base_map)


point_plot <- ggmap(base_map) + 
  theme_nothing() +
  labs(x = NULL,y = NULL) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  theme(plot.margin=unit(-c(1,1,1,1), "mm"))

together_plot <- ggmap(my_plot_trans)+  
  theme_nothing() +
  labs(x = NULL,y = NULL) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  theme(plot.margin=unit(-c(1,1,1,1), "mm"))

list_plot <- list(point_plot,together_plot)

# rayshader plot -------------
# 

rgl::clear3d()

plot_gg(list_plot, height = nrow(base_map)/200
        , width = ncol(base_map)/200, scale = 100
        , raytrace = FALSE, windowsize = c(1200, 1200),
        fov = 155.06115723, zoom = 0.09427112 
        , theta = 37.30995527, phi = 14.17542222   
        ,  max_error = 0.001, verbose = TRUE) 

#rayshader::render_camera()

# create scaling factors
scale_altitude <- 5
tmp_gps1 <- data.table::copy(tmp_gps)
tmp_gps1[, new_scale_altitude := ( time - min(time)) * scale_altitude]

# create colors
scale_color_shape_id <- viridis::viridis(n = 3)
unique_shape_id <- unique(tmp_stops$shape_id)

# add multiple trips
for(i in seq_along(unique_shape_id)){# i = unique(tmp_gps$shape_id)[1]
  
  unique_trip_id <- unique(tmp_gps1[shape_id == unique_shape_id[i]]$trip_number)
  
  for(j in unique_trip_id){ # j = unique_trip_id[1]
    
    rayshader::render_path(extent = raster::extent(tmp_gps_bbox)
                           , lat = tmp_gps1[trip_number == j & shape_id == unique_shape_id[i],]$shape_pt_lat
                           , long = tmp_gps1[trip_number == j & shape_id == unique_shape_id[i],]$shape_pt_lon
                           , altitude = tmp_gps1[trip_number == j & shape_id == unique_shape_id[i],]$new_scale_altitude
                           , zscale = 100, linewidth = 2
                           , clear_previous = F
                           #, color = "black"
                           , color = scale_color_shape_id[i]
    )
    
  }
}

### add shadow of shape in map

for(i in seq_along(unique_shape_id)){# i = unique(tmp_gps1$shape_id)[1]
  
  unique_trip_id <- unique(tmp_gps1[shape_id == unique_shape_id[i]]$trip_number)
  
  for(j in unique_trip_id){ # j = unique_trip_id[1]
    
    rayshader::render_path(extent = raster::extent(tmp_gps_bbox)
                           , lat = tmp_gps1[trip_number == j & shape_id == unique_shape_id[i],]$shape_pt_lat
                           , long = tmp_gps1[trip_number == j & shape_id == unique_shape_id[i],]$shape_pt_lon
                           , altitude = 150
                           , zscale = 100
                           , linewidth = 1
                           , clear_previous = F
                           , color = "black"
                           #, color = scale_color_shape_id[i]
    )
    
  }
}

# add stops

tmp_stops_id <- data.table::copy(tmp_stops) %>% 
  .[shape_id %in% unique(tmp_gps1$shape_id)] %>% 
  .[, new_scale_altitude := ( time - min(time)) * scale_altitude] 

tmp_stops_id[,N := .N,by = stop_id]

rayshader::render_points(extent = raster::extent(tmp_gps_bbox),
                         lat = tmp_stops_id$Y, long = tmp_stops_id$X,
                         altitude = tmp_stops_id$new_scale_altitude,
                         size = 3.5, zscale = 100,
                         clear_previous = TRUE, color = "red")

# Add Vertical Lines at Stops ---

min_time_in_window <- min(tmp_stops$time, na.rm = TRUE)
# Prepare data subset with stops, including coordinates and calculated altitude
tmp_stops_for_lines <- data.table::copy(tmp_stops) %>%
  # Calculate altitude relative to the overall minimum time in the window
  .[, new_scale_altitude := (time - min_time_in_window) * scale_altitude] 

# Get unique trip numbers from this prepared data
unique_trip_numbers_in_window <- unique(tmp_stops_for_lines$trip_number)
# Select last trip to render paths from
last_trip_number <- max(unique_trip_numbers_in_window, na.rm = TRUE)
last_trip_stops <- tmp_stops_for_lines[trip_number == last_trip_number, ]

# Loop through each stop
for (i in 1:nrow(last_trip_stops)) {
  stop_lat <- last_trip_stops$Y[i]
  stop_lon <- last_trip_stops$X[i]
  stop_alt <- last_trip_stops$new_scale_altitude[i]
  base_alt <- 1
  
  # Define the coordinates for the 2-point vertical path
  line_lats <- c(stop_lat, stop_lat)     
  line_lons <- c(stop_lon, stop_lon)     
  line_alts <- c(base_alt, round(stop_alt))
  
  # Draw the vertical line as a path
  rayshader::render_path(
    extent = raster::extent(tmp_gps_bbox), 
    lat = line_lats,
    long = line_lons,
    altitude = line_alts, 
    zscale = 100,            
    linewidth = 1,       
    clear_previous = FALSE,  
    color = "black")
}


# add labels ---

tmp_stops1 <- data.table::copy(tmp_stops) %>% 
  .[, new_scale_altitude := ( time - min(time) ) * scale_altitude] %>% 
  .[,text_hour := as.ITime(timestamp) %>% data.table::hour()] %>% 
  .[,text_min := as.ITime(timestamp) %>% data.table::minute()] %>% 
  .[,text_min := ifelse(nchar(text_min)==1,paste0("0",text_min),text_min)] %>% 
  .[,text_plot := sprintf('%s:%s',text_hour,text_min)]

tmp_stops1

# get artificial heightmap to add @ render_label function
elev_matrix <- raster::raster(nrows=808, ncols=964)
values(elev_matrix) <- 0
raster::extent(elev_matrix) <- raster::extent(tmp_gps_bbox)

# Prepare the data for start labels (one row per trip)
label_data_start <- tmp_stops1[, .SD[1], by = trip_number]

for (i in 1:nrow(label_data_start)) {
  rayshader::render_label(
    heightmap = elev_matrix,
    lat = label_data_start$Y[i],        
    long = label_data_start$X[i],         
    altitude = label_data_start$new_scale_altitude[i], 
    zscale = 100,
    textsize = 2.5,
    alpha = 0,
    adjustvec = c(2.5, 0),
    extent = attr(elev_matrix, "extent"),
    fonttype = "standard",
    text = label_data_start$text_plot[i], 
    clear_previous = ifelse(i == 1, TRUE, FALSE) 
  )
}

# Prepare the data for end labels (one row per trip)
label_data_end <- tmp_stops1[, .SD[.N], by = trip_number]

for (i in 1:nrow(label_data_end)) {
  rayshader::render_label(
    heightmap = elev_matrix,
    lat = label_data_end$Y[i],          
    long = label_data_end$X[i],         
    altitude = label_data_end$new_scale_altitude[i], 
    zscale = 100,
    textsize = 2.5,
    alpha = 0,
    adjustvec = -c(1.5, 0.35),
    extent = attr(elev_matrix, "extent"),
    fonttype = "standard",
    text = label_data_end$text_plot[i],
    clear_previous = FALSE
  )
}

# 6) saving----

rayshader::render_snapshot(filename = "figures/26_Sunday.png"
                           ,width = 1000
                           ,height = 2000)