library(shiny)
library(leaflet)
library(dplyr)

min_date <- min(df_separated$years)
max_date <- max(df_separated$years)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("mapAct", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
  sliderInput("animation", "Time:",
              min = min_date,
              max = max_date,
              value = min_date,
              step = 1,
              sep = "",
              animate = animationOptions(interval = 1000, loop = TRUE))
  )
)

server <- function(input, output){
  filteredData <- reactive({
    from <- input$animation
    till <- input$animation
    df_separated %>% filter(years >= from & years <= till)
  })
  
  output$mapAct <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data=ma_shape, color="#ff962b", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5) %>%
      setMaxBounds(lng1=shape_coordinates$xmin,lat1=shape_coordinates$ymin,
                   lng2=shape_coordinates$xmax,lat2=shape_coordinates$ymax) %>%
      addLegend(data = df_separated, pal=pal, position="bottomright", values=~ProgramID, 
                labFormat=labelFormat(prefix="Program "))
  })
  
  observe({
    leafletProxy("mapAct", data = filteredData()) %>%
      clearMarkers() %>%
      addCircleMarkers(lat=~Latitude_D, lng=~Longitude_, color=~pal(ProgramID),
                       weight=1, radius=5, fillOpacity=0.6)
  })
}

shinyApp(ui = ui, server = server)