library(data.table)
library(stringr)
library(dplyr)
library(sf)
library(leaflet)
library(mapview)
library(magick)
library(sp)

shape_file_location <- "C:/Users/Hill_T/OneDrive - Florida Department of Environmental Protection/Desktop/SEACAR Shapefiles/"

map_output <- "maps/"

# Sample Locations
point <- st_read(paste0(shape_file_location,"SampleLocations_12jan21/", "seacar_dbo_SampleLocation_Point.shp"))

# AP and NERR shapefiles
AP_shp <- st_read(paste0(shape_file_location,"APs/Florida_Aquatic_Preserves.shp"))
NERR_shp <- st_read(paste0(shape_file_location, "NERRs/Florida_National_Estuarine_Resarch_Reserves__NERR__Boundaries.shp"))

# Load subsetted and filtered SAV4 (output from SAV script)
SAV4 <- readRDS("data/SAV4.rds")

###############
## FUNCTIONS ##
###############

# Allows location of shapefile for each MA
find_shape <- function(ma){
  if (grepl("National Estuarine", ma, fixed = TRUE)){
    shape_file <- NERR_shp %>% filter(SITE_NAME==ma)
  } else if (grepl("Aquatic Preserve", ma, fixed = TRUE)) {
    shape_file <- AP_shp %>% filter(LONG_NAME==ma)
  }
  return(shape_file)
}

get_shape_coordinates <- function(ma_shape){
  bbox_list <- lapply(st_geometry(ma_shape), st_bbox)
  maxmin <- as.data.frame(matrix(unlist(bbox_list),nrow=nrow(ma_shape)))
  names(maxmin) <- names(bbox_list[[1]])
  return(maxmin)
}

sav_maps <- function(ma){
  
  # shortened MA name for file output
  ma_abrev <- gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', ma, perl = TRUE)
  
  # locate shape file for given MA
  ma_shape <- find_shape(ma)
  
  # get coordinates to set zoom level
  shape_coordinates <- get_shape_coordinates(ma_shape)
  
  # create list of LocationIDs with SAV data for each MA
  sav_locs <- SAV4 %>% filter(ManagedAreaName == ma) %>%
    distinct(LocationID)
  
  # Number of data points for each sample location (point size)
  ma_sav <- SAV4 %>% filter(ManagedAreaName == ma) %>%
    group_by(ProgramLocationID) %>%
    summarise(n_data = n())
  
  # Filtering samplelocation coordinates for each MA
  sav_df <- point %>% filter(LocationID %in% sav_locs$LocationID)
  
  # set palette
  pal <- colorFactor("plasma", sav_df$ProgramID)
  
  # Creating map
  map <- leaflet(sav_df) %>%
    addTiles() %>%
    addPolygons(data=ma_shape, color="#ff962b", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5) %>%
    setMaxBounds(lng1=shape_coordinates$xmin,lat1=shape_coordinates$ymin,
                 lng2=shape_coordinates$xmax,lat2=shape_coordinates$ymax) %>%
    addCircleMarkers(lat=~Latitude_D, lng=~Longitude_, color=~pal(ProgramID),
                     weight=1, radius=5, fillOpacity=0.6) %>% #radius=sqrt(ma_sav$n_data)/2
    addLegend(pal=pal, values=~ProgramID, labFormat=labelFormat(prefix="Program "))
  
  # map output filepath
  map_out <- paste0(map_output, ma_abrev, "_sav.png")
  
  # save file as png
  mapshot(map, file = map_out)
  
  # print completion
  print(paste0("Map exported for ", ma))
  
  return(map)
}

##################
## START SCRIPT ##
##################

managed_areas <- unique(SAV4$ManagedAreaName)

ma_exclude <- c("Florida Keys National Marine Sanctuary")

managed_areas <- !managed_areas %in% ma_exclude

for(ma in managed_areas){
  sav_maps(ma)
}