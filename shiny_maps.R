library(shiny)
library(dplyr)
library(leaflet)

shape_file_location <- "C:/Users/Hill_T/OneDrive - Florida Department of Environmental Protection/Desktop/SEACAR Shapefiles/"

# Sample Locations
point <- st_read(paste0(shape_file_location,"SAV_SampleLocations6june2023/seacar_dbo_vw_SampleLocation_Point.shp"))

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

# Gets coordinate min and max from shapefile
# This allows for accurately setting view on the map
get_shape_coordinates <- function(ma_shape){
  bbox_list <- lapply(st_geometry(ma_shape), st_bbox)
  maxmin <- as.data.frame(matrix(unlist(bbox_list),nrow=nrow(ma_shape)))
  names(maxmin) <- names(bbox_list[[1]])
  return(maxmin)
}

# plots the data if return = "maps",
# else it returns dataframe containing ProgramID, coordinates, & sample locations
get_ma_data <- function(ma, return="data"){
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
    summarise(n_data = n(), years = list(unique(Year)))
  
  # Filtering samplelocation coordinates for each MA
  sav_df <- point %>% filter(LocationID %in% sav_locs$LocationID)
  
  # Combine into a single dataframe
  combined_df <- merge(x=ma_sav, y=sav_df, by.x="ProgramLocationID", by.y="ProgramLoc")
  
  # unlist years
  combined_df$years <- lapply(combined_df$years, function(y) {
    if (is.character(y)) {
      as.numeric(unlist(strsplit(y, split = ",\\s*")))
    } else {
      y
    }
  })
  
  df_separated <- combined_df %>%
    unnest(c(years))
  
  # set palette
  pal <- colorFactor("plasma", df_separated$ProgramID)
  
  map <- leaflet() %>%
    addTiles() %>%
    addPolygons(data=ma_shape, color="#ff962b", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5) %>%
    setMaxBounds(lng1=shape_coordinates$xmin,
                 lat1=shape_coordinates$ymin,
                 lng2=shape_coordinates$xmax,
                 lat2=shape_coordinates$ymax) %>%
    addLegend(data = df_separated, pal=pal, position="bottomright", values=~ProgramID,
              labFormat=labelFormat(prefix="Program "))
  
  if (return == "map") {
    return(map)
  } else if (return == "data"){
    return(df_separated)
  }
}

managed_areas <- sort(unique(SAV4$ManagedAreaName))

# excluding managedareas without coordinate data or which lack shapefiles
ma_exclude <- c("Florida Keys National Marine Sanctuary","Indian River-Malabar to Vero Beach Aquatic Preserve",
                "Indian River-Vero Beach to Ft. Pierce Aquatic Preserve", "Banana River Aquatic Preserve",
                "Cockroach Bay Aquatic Preserve", "Mosquito Lagoon Aquatic Preserve","Nature Coast Aquatic Preserve",
                "Terra Ceia Aquatic Preserve")

managed_areas <- managed_areas[!managed_areas %in% ma_exclude]

colorPalette <- function(){
  get_ma_data("Biscayne Bay Aquatic Preserve")
}

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("mapAct", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("yearslider", "Time:",
                            min = 1999,
                            max = 2022,
                            value = 1999,
                            step = 1,
                            sep = "",
                            animate = animationOptions(interval = 1000, loop = TRUE)),
                selectInput("managedArea", "Select ManagedArea to view", 
                            choices=managed_areas, "Biscayne Bay Aquatic Preserve")),
  
)

server <- function(input, output, session){
  
  filteredData <- reactive({
    
    from <- input$yearslider
    till <- input$yearslider
    get_ma_data(input$managedArea) %>% 
      filter(years >= from & years <= till)
    
  })
  
  colorPalette <- reactive({
    get_ma_data(input$managedArea)
  })
  
  output$mapAct <- renderLeaflet({
    
    ### MAP
    get_ma_data(input$managedArea, "map")
    
  })
  
  observe({
    
    updateSliderInput(session = session, "yearslider",
                      min = min(colorPalette()$years),
                      max = max(colorPalette()$years),
                      step = 1)
    
    pal <- colorFactor("plasma", colorPalette()$ProgramID)
    
    leafletProxy("mapAct", data = filteredData()) %>%
      clearMarkers() %>%
      addCircleMarkers(lat=~Latitude_D, lng=~Longitude_, color=~pal(ProgramID),
                       weight=1, radius=7, fillOpacity=0.4)
  })
}

shinyApp(ui = ui, server = server)