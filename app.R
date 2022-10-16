library(dplyr)
library(leaflet)
library(shiny)

wms <- "https://kartta.hel.fi/ws/geoserver/avoindata/wms"
years <- c("1749", "1780", "1917", "1932", "1943", "1956", "1964", "1976", "1988")

ui <- fluidPage(
  
  tags$h2(
    HTML("<span style='color:sienna'>Historical views of Kulosaari</span>")
  ),
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #333333;
        color: white;
      },
      .shiny-input-container {
        color: snow;
      }
      label.control-label {
        color: #5f9ea0;
      }"
    ))
  ),
  
  sidebarPanel(
    sliderInput(inputId = "o",
                label = "Opacity",
                min = 0.3,
                max = 1,
                step = 0.1,
                value = 0.6),
    selectInput(inputId = "year",
                label = "Year",
                choices = years),
    width = 2
  ),
  mainPanel(
    leafletOutput("map", height = 600), 
    width = 10)
)

server <- function(input, output, session) {
  
  year_selected <- reactive({
    case_when(input$year == '1749' ~ "avoindata:Pitajakuvauskartta_1749",
              input$year == '1780' ~ "Rekognosointikartta_1780_1782",
              input$year == '1917' ~ "georeferenced",
              input$year == '1932' ~ "Ortoilmakuva_1932",
              input$year == '1943' ~ "Ortoilmakuva_1943",
              input$year == '1956' ~ "Ortoilmakuva_1956",
              input$year == '1964' ~ "Ortoilmakuva_1964",
              input$year == '1976' ~ "Ortoilmakuva_1976",
              input$year == '1988' ~ "Ortoilmakuva_1988",
              TRUE ~ '1749')
  })
  
  output$map <- renderLeaflet(
    leaflet(options = leafletOptions(zoomSnap = 0.25, zoomDelta = 0.25)) %>% 
      setView(lng = 25.0051, lat = 60.1865, zoom = 13) 
   )
  
  observe({
    req(input$o, input$year)
    
    if (year_selected() != 'georeferenced') {
      leafletProxy("map") %>%
        clearTiles() %>% 
        addTiles(attribution = 'OpenStreetMap | City of Helsinki | @ttso') %>% 
        addWMSTiles(
          wms,
          layers = year_selected(),
          options = WMSTileOptions(format = "image/png", transparent = FALSE,
                                   opacity = input$o)
        )
    } else {
      leafletProxy("map") %>%
        clearTiles() %>% 
        setView(lng = 25.0051, lat = 60.1865, zoom = 14.25) %>% 
        addTiles() %>% 
        addTiles(
          urlTemplate = "http://tuijasonkkila.fi/tiles/kulo/{z}/{x}/{y}.png",
          options = tileOptions(tms = TRUE, opacity = input$o))    
      }
  })
  
  
 }

shinyApp(ui, server)
