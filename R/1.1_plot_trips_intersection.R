# Load and read----

# load libraries
rm(list=ls())
gc(reset = TRUE)
easypackages::packages('geobr', 'magick', 'gtfs2gps', 
                       'data.table', 'sf', 'mapview', 'progressr',
                       'magrittr', 'dplyr', 'ggnewscale',
                       'ggplot2', 'ggmap', 'raster', 'terra',
                       'rayshader', 'rayrender', 'rayimage')


gps_dt <- readr::read_rds("data/ttc_intersection_gps.rds")

## Adjust data -----
# deal with midnight trips
tmp_gps <- data.table::copy(gps_dt) %>%
  .[!is.na(timestamp),] %>%
  .[,.SD[1],by = .(shape_pt_lon,shape_pt_lat)] %>% 
  .[,time := as.numeric(timestamp)] %>% 
  .[,time1 := data.table::shift(time,1,NA,"lead"),by = trip_number] %>%
  .[,diff := time1 - time] 

tmp_gps <- data.table::copy(tmp_gps) %>%
  .[!is.na(timestamp),] %>%
  .[,.SD[1],by = .(shape_pt_lon,shape_pt_lat)] %>% 
  .[,time := as.numeric(timestamp)] %>%
  .[,shape_pt_lon_end := data.table::shift(shape_pt_lon,-1,NA), by = shape_id] %>%
  .[,shape_pt_lat_end := data.table::shift(shape_pt_lat,-1,NA), by = shape_id]

tmp_stops <- data.table::copy(tmp_gps) %>%
  .[,.SD[1],by = .(shape_pt_lon,shape_pt_lat)] %>% 
  .[!is.na(cumtime) & !is.na(stop_id) & !is.na(timestamp),] %>%  
  .[,time := as.numeric(timestamp)] %>% 
  .[,altitude := 100 * time/max(time)] %>% 
  data.table::setnames(.,old = c("shape_pt_lon","shape_pt_lat")
                       , new = c("X","Y"))

view_tmp_stops <- data.table::copy(tmp_stops) %>% 
  sfheaders::sf_multipoint(.,x = "X"
                           ,  y = "Y"
                           , multipoint_id = "shape_id") %>%
  sf::st_set_crs(4326) %>% # WGS 1984
  sf::st_transform(32617) %>% # UTM zone 17N
  mapview()

# points to line
tmp_line <- data.table::copy(tmp_gps) %>%
  .[,.SD[1],by = .(shape_pt_lon,shape_pt_lat)] %>% 
  .[!is.na(cumtime) & !is.na(stop_id) & !is.na(timestamp),] %>%
  sfheaders::sf_linestring(obj = .
                           , x = "shape_pt_lon"
                           , y = "shape_pt_lat"
                           , linestring_id = "shape_id"
                           , keep = TRUE) %>%
  sf::st_set_crs(4326)

# bbox
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



## download TILE ------

osm_bbox = tmp_gps_bbox %>% 
  raster::extent() %>%
  as.vector() %>% 
  .[c(1,3,2,4)]


view_osm_bbox <- sf::st_bbox(tmp_gps_bbox) %>% 
  mapview()

## read tile -----

# Stamen map tiles are now served by Stadia maps

# Visit https://client.stadiamaps.com/signup/ to obtain an API key for stadiamap
# register_stadiamaps(key = "api key", write = TRUE)

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
  labs(x = NULL,y = NULL) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  theme(plot.margin=unit(-c(1,1,1,1), "mm"))

together_plot <- ggmap(my_plot_trans)+ 
  labs(x = NULL,y = NULL) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  theme(plot.margin=unit(-c(1,1,1,1), "mm"))

list_plot <- list(point_plot,together_plot)

# Plot rayshader -------------

rgl::clear3d()

# base

plot_gg(list_plot, height = nrow(base_map)/200
        , width = ncol(base_map)/200, scale = 100
        , raytrace = FALSE, windowsize = c(1200, 1200),
        fov = 160.50793457 , zoom = 0.06098146 , theta = -30.33646027, phi = 10.38603214   
        ,  max_error = 0.01, verbose = TRUE) 

# find desired angles in pop-up render pane, then run the following line:
# rayshader::render_camera(theta = NULL,phi = NULL,zoom = NULL,fov = NULL)

# add scaling factor
scale_altitude <- 5

tmp_gps[, new_scale_altitude := ( time - min(time) ) * scale_altitude]

# add color path ---
scale_color_shape_id <- viridis::viridis(n = 3)
unique_shape_id <- unique(tmp_stops$shape_id)

# loop though shapes
for(i in seq_along(unique_shape_id)){# i = unique(tmp_gps$shape_id)[1]
  
  unique_trip_id <- unique(tmp_gps[shape_id == unique_shape_id[i]]$trip_number)
  
  # loop through trips
  for(j in unique_trip_id){ # j = unique_trip_id[1]
    
    rayshader::render_path(extent = raster::extent(tmp_gps_bbox)
                           , lat = tmp_gps[trip_number == j & shape_id == unique_shape_id[i],]$shape_pt_lat
                           , long = tmp_gps[trip_number == j & shape_id == unique_shape_id[i],]$shape_pt_lon
                           , altitude = tmp_gps[trip_number == j & shape_id == unique_shape_id[i],]$new_scale_altitude
                           , zscale = 100
                           , linewidth = 5
                           , clear_previous = F
                           , color = scale_color_shape_id[i]
    )
    
  }
}

# add points ---
# update id & shape_id values
tmp_stops_id <- data.table::copy(tmp_stops)[shape_id %in% unique(tmp_gps$shape_id)]
tmp_stops_id[, new_scale_altitude := ( time - min(time) ) * scale_altitude]
tmp_stops_id[,N := .N,by = stop_id]

rayshader::render_points(extent = raster::extent(tmp_gps_bbox),
                         lat = tmp_stops_id[N==1]$Y, long = tmp_stops_id[N==1]$X,
                         altitude = tmp_stops_id[N==1]$new_scale_altitude,
                         size = 5, zscale = 100,
                         clear_previous = TRUE, color = "black")
# add points for the intersection
rayshader::render_points(extent = raster::extent(tmp_gps_bbox),
                         lat = tmp_stops_id[N>1]$Y, long = tmp_stops_id[N>1]$X,
                         altitude = tmp_stops_id[N>1]$new_scale_altitude,
                         size = 5, zscale = 100,
                         clear_previous = FALSE, color = "red")
# add points for the beginning of the trips
rayshader::render_points(extent = raster::extent(tmp_gps_bbox)
                         , lat = tmp_stops_id[id == 1 & shape_id == "1049212"]$Y
                         , long = tmp_stops_id[id == 1 & shape_id == "1049212"]$X
                         , altitude = tmp_stops_id[id==1 & shape_id == "1049212"]$new_scale_altitude
                         , size = 10, zscale = 100
                         , clear_previous = FALSE, color = "red")
# item above but for other shape
rayshader::render_points(extent = raster::extent(tmp_gps_bbox)
                         , lat = tmp_stops_id[id == 2 & shape_id == "1048833"]$Y
                         , long = tmp_stops_id[id == 2 & shape_id == "1048833"]$X
                         , altitude = tmp_stops_id[id==2 & shape_id == "1048833"]$new_scale_altitude
                         , size = 10, zscale = 100
                         , clear_previous = FALSE, color = "red")

# Add Vertical Lines at Stops ---
# Adjusted to use render_path function instead of render_points

# Calculate the minimum time using the 'time' column from tmp_stops
min_time_for_plot <- min(tmp_stops$time, na.rm = TRUE) 

# Prepare data subset specifically for drawing lines
# Calculate altitude relative to the min_time_for_plot
tmp_stops_for_lines <- data.table::copy(tmp_stops) %>%
  # Calculate altitude relative to the minimum time derived above
  .[, new_scale_altitude := (time - min_time_for_plot) * scale_altitude] %>%
  # Filter out stops where altitude calculation is invalid or less than base altitude
  .[!is.na(new_scale_altitude) & new_scale_altitude >= 1] 

# Loop through stops and render each vertical line

for (i in 1:nrow(tmp_stops_for_lines)) {
  stop_lat <- tmp_stops_for_lines$Y[i]
  stop_lon <- tmp_stops_for_lines$X[i]
  stop_alt <- tmp_stops_for_lines$new_scale_altitude[i]
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
    linewidth = 1, # Adjust thickness if needed (e.g., 0.5)      
    clear_previous = FALSE,  # Add cumulatively
    color = "black")
}

# Add paths at ground level ---

for(i in seq_along(unique_shape_id)){# i = unique(tmp_gps$shape_id)[1]
  
  unique_trip_id <- unique(tmp_gps[shape_id == unique_shape_id[i]]$trip_number)
  
  for(j in unique_trip_id){ # j = unique_trip_id[1]
    
    rayshader::render_path(extent = raster::extent(tmp_gps_bbox)
                           , lat = tmp_gps[trip_number == j & shape_id == unique_shape_id[i],]$shape_pt_lat
                           , long = tmp_gps[trip_number == j & shape_id == unique_shape_id[i],]$shape_pt_lon
                           , altitude = 150
                           , zscale = 100
                           , linewidth = 1
                           , clear_previous = F
                           , color = "black"
                           #, color = scale_color_shape_id[i]
    )
    
  }
}

# prepare data to add lables
tmp_stops1 <- data.table::copy(tmp_stops_id) %>% 
  .[,timestamp := timestamp] %>% 
  .[,text_hour := data.table::hour(as.ITime(timestamp))] %>% 
  .[,text_min := data.table::minute(as.ITime(timestamp))] %>% 
  .[,text_min := ifelse(nchar(text_min)==1,paste0("0",text_min),text_min)] %>% 
  .[,text_plot := sprintf('%s:%s',text_hour,text_min)]


# get artificial heightmap to add @ render_label function
elev_matrix <- raster::raster(nrows=808, ncols=964)
values(elev_matrix) <- 0
raster::extent(elev_matrix) <- raster::extent(tmp_gps_bbox)

# add label, shape == 1049212 | start trip
rayshader::render_label(heightmap = elev_matrix
                        , lat = tmp_gps[shape_id == "1049212" & id == 1,]$shape_pt_lat+0.04
                        , long = tmp_gps[shape_id == "1049212" & id == 1,]$shape_pt_lon-0.1
                        , altitude = tmp_gps[shape_id == "1049212"& id == 1,]$new_scale_altitude + 3300
                        , zscale = 100
                        , textsize = 3
                        , adjustvec = c(0,-0.5)
                        , alpha = 0
                        , extent = attr(elev_matrix, "extent")
                        , fonttype = "standard"
                        , text = tmp_stops1[shape_id == "1049212"& id == 1,]$text_plot
                        , clear_previous = T)
# add label, shape == 1048833 | start trip
rayshader::render_label(heightmap = elev_matrix
                        , lat = tmp_gps[shape_id == "1048833" & id == 2,]$shape_pt_lat-0.0155
                        , long = tmp_gps[shape_id == "1048833" & id == 2,]$shape_pt_lon+0.015
                        , altitude =   tmp_gps[shape_id == "1048833"& id == 2,]$new_scale_altitude -4800
                        , zscale = 100
                        , textsize = 3
                        , linewidth = 0
                        , alpha = 0
                        , adjustvec = c(-.5,-2.5)
                        , extent = attr(elev_matrix, "extent")
                        , fonttype = "standard"
                        , text = tmp_stops1[shape_id == "1048833"& id == 2,]$text_plot
                        , clear_previous = F)
# add label, shape == 1049212 | end trip
rayshader::render_label(heightmap = elev_matrix
                        , lat = tmp_gps[shape_id == "1049212",.SD[.N]]$shape_pt_lat+0
                        , long = tmp_gps[shape_id == "1049212",.SD[.N]]$shape_pt_lon-0.01
                        , altitude =   tmp_gps[shape_id == "1049212",.SD[.N]]$new_scale_altitude -6500
                        , zscale = 100
                        , textsize = 3
                        , linewidth = 0
                        , alpha = 0
                        , adjustvec = c(-0.25,-2)
                        , extent = attr(elev_matrix, "extent")
                        , fonttype = "standard"
                        , text = tmp_stops1[shape_id == "1049212",.SD[.N]]$text_plot
                        , clear_previous = F)
  # add label, shape == 1048833 | end trip
  rayshader::render_label(heightmap = elev_matrix
                          , lat = tmp_gps[shape_id == "1048833",.SD[.N]]$shape_pt_lat
                          , long = tmp_gps[shape_id == "1048833",.SD[.N]]$shape_pt_lon
                          , altitude =   tmp_gps[shape_id == "1048833",.SD[.N]]$new_scale_altitude -4750
                          , zscale = 100
                          , textsize = 3
                          , linewidth = 0
                          , alpha = 0
                          , adjustvec = c(+1.25,-2)
                          , extent = attr(elev_matrix, "extent")
                          , fonttype = "standard"
                          , text = tmp_stops1[shape_id == "1048833",.SD[.N]]$text_plot
                          , clear_previous = F)

# save ----
dir.create("figures")
rayshader::render_snapshot(filename = "figures/intersection_ttc_final.png"
                           ,width = 500
                           ,height = 500)