rm(list=ls())
gc(reset = TRUE)

install.packages('cancensus')
install.packages("sf")
library("cancensus")
library("sf")


#cancensus::set_cancensus_api_key("CensusMapper_08b53dd19db8e68621df4b7b19853e48", install = TRUE)
options(cancensus.cache_path = "data/census_cache")

census_dataset <- "CA21"

toronto_csd_data <- cancensus::get_census(
  dataset = census_dataset,
  regions=list(CSD = "3520005"),
  level = "CSD",
  vectors = c("v_CA21_1","v_CA21_6","v_CA21_7","v_CA21_906"),
  geo_format = "sf",
  use_cache = TRUE
)

# Get Census Tracts (CTs) within Toronto
toronto_cts <- cancensus::get_census(
  dataset = census_dataset,
  regions = list(CSD = "3520005"),
  level = "CT",
  vectors = c("v_CA21_1","v_CA21_6","v_CA21_7","v_CA21_906"),
  geo_format = "sf",
  use_cache = TRUE
)

# Get Dissemination Areas (DAs) within Toronto
toronto_das <- cancensus::get_census(
  dataset = census_dataset,
  regions = list(CSD = "3520005"),
  level = "DA",
  vectors = c("v_CA21_1","v_CA21_6","v_CA21_7","v_CA21_906","v_CA21_1100","v_CA21_1121"),
  geo_format = "sf",
  use_cache = TRUE
)

# Write out the data to Shapefiles
setwd("C:/Users/julia/Documents/ArcGIS/Projects/IndependentStudy")
sf::st_write(toronto_csd_data, "Toronto_CSD.shp", delete_dsn = TRUE)
sf::st_write(toronto_cts, "Toronto_CTs.shp", delete_dsn = TRUE)
sf::st_write(toronto_das, "Toronto_DAs.shp", delete_dsn = TRUE)