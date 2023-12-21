library(shiny)
library(dplyr)
library(leaflet)
library(data.table)
library(stringr)
library(sf)
library(tidyr)
library(mapview)
library(magick)
library(htmlwidgets)

source("load_shape_samples.R")

seacar_data <- "C:/Users/Hill_T/OneDrive - Florida Department of Environmental Protection/Desktop/SEACAR Data/SEACARdata"

#load oyster data
files <- list.files(here::here(seacar_data))
oyster_file <- str_subset(files, "Oyster")

oyster <- fread(paste0(seacar_data, "/", oyster_file), sep='|')

oyster <- oyster %>% filter(Include==1, MADup==1)

##############
# create list of ProgramIDs with oyster data for each MA
oyster_programs <- unique(oyster$ProgramID)

# Number of data points for each sample location (point size)
ma_oyster <- oyster %>%
  group_by(ProgramLocationID, ProgramID, ProgramName, LocationID) %>%
  summarise(n_data = n(), years = list(sort(unique(Year))), params = list(unique(ParameterName)))

# Filtering samplelocation coordinates for each MA
oyster_df <- point %>% filter(ProgramID %in% oyster_programs)

# Combine into a single dataframe
combined_df <- merge(x=ma_oyster, y=oyster_df, by.x=c("ProgramLocationID","ProgramID", "LocationID"), by.y=c("ProgramLoc", "ProgramID", "LocationID"))

df_separated <- combined_df

oimmp_pal <- colorFactor("Set3", oimmp_boundaries$Region)
pal2 <- colorFactor("plasma", df_separated$ProgramID)
rad2 <- sqrt(df_separated$n_data)/5

df_separated <- df_separated %>%
  mutate(popup = paste("ProgramID: ", ProgramID, "<br> ProgramName: ", ProgramName, "<br> LocID: ", LocationID, "<br> ProgLocID: ", ProgramLocationID, 
                       "<br> N_Data: ", n_data, "<br> Years: ", years, "<br> params: ", params),
         label = paste0(ProgramID, ": ", ProgramName, " - LocID: ", LocationID))

map2 <- leaflet(df_separated) %>%
  addTiles() %>%
  addLegend(title = "Program ID",
            pal=pal, 
            position="bottomright", 
            values=~ProgramID,
            labFormat=labelFormat(prefix="Program ")) %>%
  addCircleMarkers(lat=~Latitude_D, lng=~Longitude_, color=~pal2(ProgramID),
                   weight=1, radius=rad2, fillOpacity=0.4,
                   popup = ~popup,
                   label = ~label)

saveWidget(map2, file="Oyster_All_programs_full_info.html")

boundary_map <- leaflet(df_separated) %>%
  addTiles(group = "Default") %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels, group = "Positron by CartoDB") %>%
  addLegend(title = "Program ID",
            pal=pal2, 
            position="bottomright", 
            values=~ProgramID,
            labFormat=labelFormat(prefix="Program "))  %>%
  addLegend(title = "OIMMP Boundary",
            pal=oimmp_pal,
            position="bottomleft",
            values = ~oimmp_boundaries$Region) %>%
  addMapPane("background", zIndex = 400) %>%
  addMapPane("foreground", zIndex = 500) %>%
  addPolygons(data=AP_shp, color="#4e809c", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.2,
              group="AP Boundary", options = pathOptions(pane = "background")) %>%
  addPolygons(data=NERR_shp, color="#fc515c", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.2,
              group="NERR Boundary", options = pathOptions(pane = "background")) %>%
  addPolygons(data=oimmp_boundaries, color = "#F0F0F0", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.4, fillColor = ~oimmp_pal(Region), 
              group="OIMMP Boundary", options = pathOptions(pane = "background")) %>%
  addLayersControl(
    overlayGroups = c("AP Boundary", "NERR Boundary", "OIMMP Boundary"),
    baseGroups = c("Default", "Positron by CartoDB"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  addCircleMarkers(lat=~Latitude_D, lng=~Longitude_, color=~pal2(ProgramID),
                   weight=1, radius=rad2, fillOpacity=0.4,
                   popup = ~popup,
                   label = ~label,
                   options = pathOptions(pane = "foreground"))

saveWidget(boundary_map %>% hideGroup(c("NERR Boundary", "AP Boundary", "OIMMP Boundary")), file="Oyster_All_programs_full_info.html")

# boundary_map %>% hideGroup(c("NERR Boundary", "AP Boundary", "OIMMP Boundary"))