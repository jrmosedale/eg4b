#
# test shiny app using WMS tiles

#

library(shiny)
library(leaflet)
library(magrittr)
library(colorspace)
library(rgeos)
library(ggplot2)
library(shinyjs)
library(rgdal)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("empty"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         leafletOutput("map")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$map<-renderLeaflet({
    leaflet() %>% 
      setView(lng=-5.0, lat = 50.2, zoom = 9)  %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      addWMSTiles(
        "http://environment.data.gov.uk/ds/wms?SERVICE=WMS&INTERFACE=ENVIRONMENT--92912a4f-d465-11e4-8687-f0def148f590",
        layers = "ea_wms_risk_of_flooding_from_surface_water_extent_0_1_percent_annual_chance",
        options = WMSTileOptions(format = "image/png", transparent = TRUE),
        tileOptions(tms = TRUE),
        attribution = "")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

# Other EA services
#   rpa_wms_crop_map_of_england_2017
# FWA: http://environment.data.gov.uk/ds/wms?SERVICE=WMS&INTERFACE=ENVIRONMENT--87e5d78f-d465-11e4-9343-f0def148f590
# Risk F from multiple sources: http://environment.data.gov.uk/ds/wms?SERVICE=WMS&INTERFACE=ENVIRONMENT--8651d5af-be8c-4990-8ac9-c4ecd3cd1d6a

