# Load ----

rm(list=ls())
gc(reset = TRUE)

install.packages('easypackages')
easypackages::packages(, 'gtfs2gps'
                       , 'data.table'
                       , 'magrittr'
                       , 'ggplot2'
                       , 'rayshader'
                       , 'progressr'
                       , 'pbapply'
                       , 'cancensus'
                       , 'sf'
                       , 'dplyr')

# GPS filter ----

# read Toronto boundary
toronto_bound <- readr::read_rds("data/toronto_bound_CSD.rds")

# ttc data
ttc_path <- "data-raw/ttc_gtfs.zip"
ttc_gtfs <- gtfstools::read_gtfs(path = ttc_path)
ttc_gtfs <- gtfstools::filter_by_weekday(ttc_gtfs,"wednesday")

# gtfs2gps 
dir.create("data/gps/")
dir.create("data/gps/ttc")

progressr::with_progress(
  gtfs2gps::gtfs2gps(gtfs_data = ttc_gtfs,
                     parallel = FALSE,
                     filepath = "data/gps/ttc/")
)

# filter

ttc_files <- list.files ("data/gps/ttc/",full.names = TRUE)
ttc_stops <- pbapply::pblapply(ttc_files,function(i){
  # i = emtu_files[1]
  tmp <- data.table::fread(i, select = c('shape_id','trip_id','stop_id',
                                         'timestamp', 'dist',
                                         'shape_pt_lat', 'shape_pt_lon'))
  tmp <- tmp[!is.na(stop_id) & dist != 0]
  return(tmp)
  
}) %>% data.table::rbindlist()


all_stops <- ttc_stops
all_stops[, stop_id := as.character(stop_id)]

# Find bus stops inside Toronto -----

# total bus stop_ids 
uniqueN(all_stops$stop_id) 

unique_stops_sf <- sfheaders::sf_multipoint(
  obj = all_stops[,.SD[1],by = .(stop_id)]
  ,x = "shape_pt_lon"
  ,y = "shape_pt_lat"
  ,multipoint_id = "stop_id"
  ,keep = FALSE)

unique_stops_sf <- sf::st_set_crs(unique_stops_sf,4326)

tmp_id <- sf::st_within(x = unique_stops_sf
                        ,y = toronto_bound
                        ,sparse = FALSE)

# total stops inside Toronto
sum(tmp_id)

unique_stops <- unique_stops_sf[which(tmp_id),]$stop_id
unique_stops <- as.character(unique_stops)

# apply filter
all_stops_filtered <- all_stops[stop_id %in% unique_stops,]

readr::write_rds(all_stops_filtered,"data/all_stops_toronto_filtered.rds",compress = "gz")

# 2) Freq by time -----
rm(list=ls())
gc(reset = TRUE)

all_stops <- readr::read_rds("data/all_stops_toronto_filtered.rds")

# adjust time
all_stops[,time_to_sec := gtfstools:::cpp_time_to_seconds(timestamp)]
all_stops[,minu_time := round(time_to_sec/60,1)]
all_stops <- all_stops[minu_time < 1440]

# add time classes ------
# 60 min

tp2 <- as.character(0:23)
tp2[nchar(tp2) == 1] <- paste0("0",tp2[nchar(tp2) == 1])

label_60min <- paste0(tp2,c(":00"))
all_stops[, time_60min := cut(minu_time
                                , breaks = seq(0,1440
                                               ,length.out = length(label_60min)+1)
                                , right = FALSE
                                , labels = label_60min)]
# 30 min
tp2 <- as.character(rep(0:23,each = 2))
tp2[nchar(tp2) == 1] <- paste0("0",tp2[nchar(tp2) == 1])

label_30min <- paste0(tp2,c(":00",":30"))
all_stops[, time_30min := cut(minu_time
                                , breaks = seq(0,1440
                                               ,length.out = length(label_30min)+1)
                                , right = FALSE
                                , labels = label_30min)]
# 15 min
tp2 <- as.character(rep(0:23,each = 4))
tp2[nchar(tp2) == 1] <- paste0("0",tp2[nchar(tp2) == 1])

label_15min <- paste0(tp2,c(":00",":15",":30",":45"))
all_stops[, time_15min := cut(minu_time
                                , breaks = seq(0,1440
                                               ,length.out = length(label_15min)+1)
                                , right = FALSE
                                , labels = label_15min)]

# 10 min
tp2 <- as.character(rep(0:23,each = 6))
tp2[nchar(tp2) == 1] <- paste0("0",tp2[nchar(tp2) == 1])

label_10min <- paste0(tp2,c(":00",":10",":20",":30",":40",":50"))
all_stops[, time_10min := cut(minu_time
                                , breaks = seq(0,1440
                                               ,length.out = length(label_10min)+1)
                                , right = FALSE
                                , labels = label_10min)]

# 5 min
tp1 <- as.character(seq(0,55,by = 5))
tp1[nchar(tp1) == 1] <- paste0("0",tp1[nchar(tp1) == 1])
tp2 <- as.character(rep(0:23,each = 12))
tp2[nchar(tp2) == 1] <- paste0("0",tp2[nchar(tp2) == 1])
label_05min <- paste0(tp2,":",tp1)

all_stops[, time_05min := cut(minu_time
                                , breaks = seq(0,1440
                                               ,length.out = length(label_05min)+1)
                                , right = FALSE
                                , labels = label_05min)]

rm(list = c("label_10min","label_15min","label_30min","label_60min"
            ,"label_05min","tp1","tp2"))

# calculate frequency ----

all_stops[,N_60min := .N,by = .(stop_id,time_60min)]
all_stops[,N_30min := .N,by = .(stop_id,time_30min)]
all_stops[,N_15min := .N,by = .(stop_id,time_15min)]
all_stops[,N_10min := .N,by = .(stop_id,time_10min)]
all_stops[,N_05min := .N,by = .(stop_id,time_05min)]

all_stops <- data.table::melt.data.table(
  data = all_stops
  ,id.vars = c('stop_id','shape_pt_lat',  'shape_pt_lon')
  ,measure.vars = list(
    "time" = c('time_60min','time_30min','time_15min', 'time_10min', 'time_05min')
    ,"N" = c('N_60min','N_30min','N_15min', 'N_10min', 'N_05min')
  ))

all_stops[,time_interval := fcase(
  variable == 1,"60 min",
  variable == 2,"30 min",
  variable == 3,"15 min",
  variable == 4,"10 min",
  variable == 5,"05 min"  )]

#all_stops[,variable := NULL]

all_stops[1]
all_stops$time %>% unique() %>% sort()

# stops to sf

stops_sf <- sfheaders::sf_point(obj = all_stops[,.SD[1],by = .(stop_id)]
                                ,x = "shape_pt_lon"
                                ,y = "shape_pt_lat"
                                ,keep = TRUE)
stops_sf <- sf::st_set_crs(stops_sf,4326)

# Create Hexagonal Grid ----

# Project Toronto boundary and stops to UTM Zone 17N

toronto_bound <- readr::read_rds("data/toronto_bound_CSD.rds")
toronto_crs <- 26917
toronto_bound_proj <- sf::st_transform(toronto_bound, toronto_crs)
stops_sf_proj <- sf::st_transform(stops_sf, toronto_crs)

# Target Area ~ 0.11 km^2 (similar to H3 level 9) = 110,000 m^2
target_area_m2 <- 110000
target_side_length <- sqrt(2 * target_area_m2 / (3 * sqrt(3))) # approx 205m
target_cellsize <- sqrt(3) * target_side_length # approx 355m

# Use the bounding box of the projected boundary to create grid
grid_poly <- sf::st_make_grid(toronto_bound_proj,
                              cellsize = target_cellsize,
                              what = "polygons",
                              square = FALSE)

# Convert to sf object
toronto_hex_grid_sf <- sf::st_sf(geometry = grid_poly)

# Add a unique hex_id column
toronto_hex_grid_sf$hex_id <- 1:nrow(toronto_hex_grid_sf)

# Assign CRS
toronto_hex_grid_sf <- sf::st_set_crs(toronto_hex_grid_sf, toronto_crs)

# Clip the grid to the Toronto boundary
toronto_hex_grid_proj <- sf::st_intersection(toronto_hex_grid_sf, toronto_bound_proj)

# Select only the hex_id and geometry after intersection
toronto_hex_grid_proj <- select(toronto_hex_grid_proj, hex_id, geometry)

# Retrieve Census Data ----
options(cancensus.cache_path = "data/cancensus_cache")
census_dataset <- "CA21"
toronto_da_data <- cancensus::get_census(
  dataset = census_dataset,
  regions = list(CSD = "3520005"), # Toronto CSD
  level = "DA",
  vectors = c("v_CA21_1","v_CA21_906"), # Population and Median Total Household Income
  geo_format = "sf",
  use_cache = TRUE
)

# Project DA data to the same CRS as hex grid
toronto_da_data_proj <- sf::st_transform(toronto_da_data, toronto_crs)

toronto_da_data_proj <- toronto_da_data_proj %>%
  dplyr::rename(
    Population2021 = `v_CA21_1: Population, 2021`,
    MedianTotalIncome = `v_CA21_906: Median total income of household in 2020 ($)`
  )

# Calculate DA area
toronto_da_data_proj$da_area <- sf::st_area(toronto_da_data_proj)

# Apportion Census Data to Hex Grid ----

# Ensure geometries are valid
toronto_hex_grid_proj <- sf::st_make_valid(toronto_hex_grid_proj)
toronto_da_data_proj <- sf::st_make_valid(toronto_da_data_proj)

# Intersect hexagons and DAs
intersection_sf <- sf::st_intersection(toronto_hex_grid_proj, toronto_da_data_proj)

# Calculate area of each intersection polygon
intersection_sf$intersection_area <- sf::st_area(intersection_sf)

# Convert sf to data.table
intersection_dt <- data.table::setDT(sf::st_drop_geometry(intersection_sf))

# Calculate apportioned values per hexagon
hex_apportioned_data <- intersection_dt[, .(
  # Area-weighted income: sum(Income * intersection_area) / sum(intersection_area)
  weighted_income = sum(MedianTotalIncome * intersection_area, na.rm = TRUE) / sum(intersection_area, na.rm = TRUE),
  # Apportioned population: sum(Population_Density * intersection_area)
  apportioned_pop = sum( (Population2021 / fifelse(da_area > units::set_units(0,"m^2"), units::drop_units(da_area), 1) ) * units::drop_units(intersection_area), na.rm = TRUE)
), by = hex_id]

# Handle potential NaNs if sum(intersection_area) was 0 for any hex
hex_apportioned_data$weighted_income[is.nan(hex_apportioned_data$weighted_income)] <- 0 # Or NA
hex_apportioned_data$apportioned_pop <- round(hex_apportioned_data$apportioned_pop) # Round population


# Calculate Income Decile Breaks
breaks <- quantile(hex_apportioned_data$weighted_income, probs = seq(0, 1, 0.1), na.rm = TRUE)

# Assign income deciles
hex_apportioned_data[, income_decile := cut(weighted_income,
                                            breaks = breaks,
                                            labels = 1:10,
                                            include.lowest = TRUE,
                                            right = TRUE)]

# Convert factor to numeric
hex_apportioned_data$income_decile <- as.numeric(as.character(hex_apportioned_data$income_decile))
# Assign NA to hexagons with no income data
hex_apportioned_data$income_decile[is.na(hex_apportioned_data$weighted_income)] <- NA

# Join apportioned data back to hex grid
toronto_hex_grid_final_proj <- merge(toronto_hex_grid_proj, hex_apportioned_data, by = "hex_id", all.x = TRUE)

# Transform final grid back to WGS84
toronto_hex_grid_final_wgs84 <- sf::st_transform(toronto_hex_grid_final_proj, 4326)


# Spatial Join Stops to Hexagons ---
stops_with_hex <- sf::st_join(stops_sf_proj, toronto_hex_grid_final_proj, join = sf::st_within)

# Select relevant columns (stop_id, hex_id, income_decile, apportioned_pop)
stops_hex_lookup <- sf::st_drop_geometry(stops_with_hex) %>%
  select(stop_id, hex_id, income_decile, apportioned_pop) %>%
  data.table::setDT()

# Allocate Hexagon Info to Stops Frequency Data ---
# Merge hex_id from the lookup table into the frequency data
all_stops[stops_hex_lookup, on = "stop_id", hex_id := i.hex_id]

# Sum frequency (N) per hexagon, time interval, and time bin
hex_freq_agg <- all_stops[!is.na(hex_id),
                          list(N = sum(N, na.rm = TRUE)),
                          by = .(hex_id, time, time_interval)]

# Allocate Income/Population Info ----

# Merge the aggregated frequency data with the hexagon data (income decile, population)
hex_data_for_merge <- hex_apportioned_data[, .(hex_id, apportioned_pop, weighted_income, income_decile)]
setnames(hex_data_for_merge, 
         old = c("apportioned_pop", "weighted_income", "income_decile"),
         new = c("total_pop", "avg_inc", "decil_ind"))

final_hex_freq_data <- merge(hex_freq_agg, hex_data_for_merge, by = "hex_id", all.x = TRUE)

# Merge final data with hex grid geometry
toronto_hex_freq_sf_proj <- merge(toronto_hex_grid_final_proj, final_hex_freq_data, by = "hex_id", all.x = TRUE)

# Convert back to WGS84
toronto_hex_freq_sf <- sf::st_transform(toronto_hex_freq_sf_proj, 4326)

# Save the final sf object

readr::write_rds(x = toronto_hex_freq_sf,
                 file = "data/toronto_hex_freq_sf.rds",
                 compress = "gz")


# Values for Article ----

rm(list=ls())
gc(reset = TRUE)

# Load the processed data
toronto_hex_freq_sf <- readr::read_rds("data/toronto_hex_freq_sf.rds")
data.table::setDT(toronto_hex_freq_sf) # Convert sf to data.table for faster processing

# check hexagons in morning peak by income group
tmp <- toronto_hex_freq_sf[time_interval == "60 min",] %>% 
  .[time %in% c("06:00","07:00","08:00"),peak := "Morning"] %>% 
  .[!is.na(peak),] %>% 
  .[,decil_class := fcase(decil_ind %in% 9:10,"20p_richest"
                          , decil_ind %in% 1:5,"50p_poorest")] %>% 
  .[!is.na(decil_class),] %>% 
  .[,list("N_vehicles" = sum(N), "N_hex" = .N, "Minutes" = 180),by = .(peak,decil_class)]

tmp[,vehicles_by_hex_by_minute := N_vehicles / (N_hex * Minutes)]
tmp[,vehicles_by_hex := N_vehicles / (N_hex )]
tmp[,prop_vehicles_by_hex := round(100 * vehicles_by_hex / min(vehicles_by_hex))]
tmp[]

tmp[decil_class == '20p_richest']$vehicles_by_hex / tmp[decil_class == '50p_poorest']$vehicles_by_hex 
#> 4839.267 / 3225.112 = 1.500496

# check mean in morning peak by income group
tmp <- toronto_hex_freq_sf[time_interval == "05 min",] %>% 
  .[time %in% c("06:00","07:00","08:00"),peak := "Morning"] %>% 
  .[!is.na(peak),] %>% 
  .[,hour :=  stringr::str_split(time,":",n = 2,simplify = TRUE)[1],by = .(hex_id,time)] %>% 
  .[,minute :=  stringr::str_split(time,":",n = 2,simplify = TRUE)[2],by = .(hex_id,time)] %>% 
  .[,time_minute := as.numeric(hour) * 60 + as.numeric(minute)] %>% 
  .[,decil_class := fcase(decil_ind %in% 9:10,"20p_richest"
                          , decil_ind %in% 1:5,"50p_poorest")] %>% 
  .[!is.na(decil_class),]

tmp <- tmp[,weighted.mean(x = time_minute,w = N),by = .(decil_class)]

tmp[,hour := V1%/%60]
tmp[,minute := (V1 - hour*60)]
tmp[,time := paste0(hour,":",round(minute,0))]
tmp[]

tmp[decil_class == '20p_richest']$vehicles_by_hex / tmp[decil_class == '50p_poorest']$vehicles_by_hex


# Plots ----

rm(list=ls())
gc(reset = TRUE)

# Load data
toronto_hex_freq_sf <- readr::read_rds("data/toronto_hex_freq_sf.rds")
data.table::setDT(toronto_hex_freq_sf) # Convert to data.table

# Remove hexagons with NA or 0 values
toronto_hex_freq_sf <- toronto_hex_freq_sf[!is.na(total_pop) & total_pop > 0 & !is.na(decil_ind)]

# save tmp data
vec <- unique(toronto_hex_freq_sf$time_interval)

list_plots <- lapply(seq_along(vec),function(i){ # i = 4
  
  tmp <- toronto_hex_freq_sf[total_pop > 0 &
                       time_interval == vec[i] & 
                       !is.na(time ),] %>% 
    .[,weighted.mean(N,total_pop),by = .(time,decil_ind )]
  
  fixed_time <- c("00:00","04:00","08:00","12:00","16:00","20:00","23:00")
  
  
  plot <- ggplot(tmp)+
    geom_tile(aes(x = time,y= as.factor(decil_ind),fill = V1))+
    scale_x_discrete(breaks = fixed_time,labels = fixed_time, drop = TRUE)+
    labs(title = vec[i]
         ,x = NULL
         ,y = "Income Decile"
         , fill = "Frequency of \nbus stops")+
    viridis::scale_fill_viridis()+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
  
  return(plot)
})


library(patchwork)

(list_plots[[1]] | list_plots[[2]])/
  (list_plots[[3]] | list_plots[[4]])

list_plots[[4]]

# rayshader ----
future::plan("multisession", workers = 19)

tmp <- toronto_hex_freq_sf[total_pop > 0 &
                     time_interval == "10 min" & 
                     !is.na(time ),] %>% 
  .[,weighted.mean(N,total_pop),by = .(time,decil_ind )]   # depois

fixed_time <- c("00:00","04:00","08:00","12:00","16:00","20:00","23:00")


plot <- ggplot(tmp)+
  geom_tile(aes(x = time,y= as.factor(decil_ind),fill = V1))+
  scale_x_discrete(breaks = fixed_time,labels = fixed_time, drop = TRUE)+
  coord_cartesian(expand = FALSE)+
  labs(title = NULL
       ,x = NULL
       ,y = "Income decile")+
  scale_fill_continuous(type = "viridis",direction = +1, name = "Mean frequency\nof vehicles at\npublic transport\nstops by every\n10 min.")+
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", colour = "white"),
    axis.text.x = element_text(angle = 0), 
    legend.position = "right",
    legend.text.position = "left",
    legend.title.position = "bottom",
    legend.title.align = 0.5, 
    legend.title = element_text(size=8), 
    legend.key.width = unit(0.5, "cm")
  ) +
  guides(fill = guide_colourbar())
plot

ggplot2::ggsave(plot
                ,filename = "figures/10min_freq_2d_WednesdayFix.png"
                ,width = 10
                ,height = 8
                ,dpi = 300
                ,scale = 0.65)

rayshader::plot_gg(ggobj = plot
                   , multicore = TRUE
                   , width = 5
                   , height = 5
                   , scale = 250
                   , windowsize = c(1400,866)
                   , zoom = 0.5391094   
                   , phi = 30.4472961      
                   , theta = -23.2254651    )

# find angle view
# rayshader::render_camera(theta = NULL,phi = NULL,zoom = NULL,fov = NULL)
rayshader::render_snapshot(filename = "figures/10min_freq_3d_rayshader_Wednesdayfix.png"
                           ,width = 1000
                           ,height = 1000
)