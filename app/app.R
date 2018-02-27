# Displays Ecosystem service layers by UI management catchment area
#
#
################################################
# Libraries
###############################################
library(shiny)
if (!require("shinydashboard")) install.packages("shinydashboard")
library("shinydashboard")
library(sf)
library(raster)
library(leaflet)
library(magrittr)
library(colorspace)
library(rgeos)
library(ggplot2)
library(shinyjs)
library(rgdal)
library(RColorBrewer)
library(shinyjs)
library(V8)

################################################
# Directories & online data
###############################################
in.root<-"/Users/jm622/apps/fal/"
dir_in<-paste(in.root,"www/",sep="")

#library(jsonlite)
#res <- fromJSON('https://ons-inspire.esriuk.com/arcgis/rest/services/Postcodes/NSPL_Latest_Centroids/MapServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json',
#               simplifyDataFrame =TRUE)

# list ref: http://r4ds.had.co.nz/lists.html 
################################################
# FUNCTIONS CALLED BY SERVER
###############################################


# Calculate cell number from lon lat values
lonlat_to_cellnumber<-function(r,x,y,zone=0){ #http://stackoverflow.com/questions/18639967/converting-latitude-and-longitude-points-to-utm
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+init=epsg:4326")  ## lat lon
  xy <- spTransform(xy, CRS("+init=epsg:3857")) # convert to raster xy
  cell <- cellFromXY(r, c(xy$X,xy$Y))
  return(cell)
}

# Identify locations meeting certain criteria
# inputs: vectors of raster stack layers and their respective min/max values
#  vars<-c(1,2,3);maxs<-c(100,100,25); mins<-c(75,75,0); r.stack<-r100.stack

create_search_map<-function(vars,mins,maxs,r.stack) {
  template.r<-raster(paste(dir_in,"cornwall_land_100m.tif",sep=""))
  #plot(template.r,color="red")
  #template.r<-projectRaster(template.r,crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",method="ngb")  
  new.r<-template.r
  #new.r<-calc(new.r,fun=function(x,y){ifelse(x==1,0,x)})
  for (n in vars){
    r<-raster(r100.stack,n)
    #r<-projectRaster(r,crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",method="ngb")  
    # All cells start selected = 1 - lose those that do not meet criteria in any layer
    new.r<-overlay(new.r,r,fun=function(x,y){ifelse(y<mins[n] | y>maxs[n],0,x)}) 
    #plot(new.r,main=n)
  } # end for 
  #plot(new.r)
  new.r<-round(new.r,0)
  new.r<-mask(new.r,template.r)
  return(new.r)
  
}# create_search_map

# Plot histogram
plothist<-function(values){
  sel<-which(!is.na(values))
  values<-values[sel]
  numcells<-length(values)
  mnx<-min(values,na.rm=TRUE)
  mxx<-max(values,na.rm=TRUE)
  #f <- hist(values, maxpixels=length(values),breaks=50) # calculate from all cells of raster
  f <- hist(values,breaks=50) 
  #abline(v=100, col="blue",lwd=3)
  dat <- data.frame(counts= ((f$counts/numcells)*100),breaks = f$mids)
  ggplot(dat, aes(x = breaks, y = counts, fill =counts)) + ## Note the new aes fill here
    geom_bar(stat = "identity",alpha = 0.8,fill="blue")+
    xlab("Service value")+ ylab("%")+
    scale_x_continuous(breaks = seq(0,100,20),
                       labels = seq(0,100,20) )  
}

twohist<-function(values.area,values.all){
  dat1<-data.frame(freq= values.all)
  dat1$Area<-"All Cornwall"
  # For sub-area 
  sel.area<-which(!is.na(values.area))
  values.area<-values.area[sel.area]
  numcells<-length(values.area)
  mnx<-min(values.area,na.rm=TRUE)
  mxx<-max(values.area,na.rm=TRUE)
  areah <- hist(values.area,breaks=seq(0,100,10)) 
  dat2 <- data.frame(freq= ((areah$counts/numcells)*100),value = areah$mids)
  dat2$Area<-"Catchment"
  
  # Combine into singel dataframe for plotting
  dat1$value<-areah$mids
  dat<-rbind(dat2,dat1) 
  
  ggplot(dat, aes(x = value, y = freq, fill = Area)) + 
     geom_bar(stat = "identity",alpha = 0.8, position='dodge') +
    xlab("Service value")+ ylab("%")+
    scale_x_continuous(breaks = seq(0,100,20),
                       labels = seq(0,100,20) )  
}



catch_data<-function(catchmask,datastack){
  catchdata<-mask(datastack,catchmask)
}



############################################
# Setup - data and file processing
############################################
# Ecosysem service names
service_names<-c("Biodiversity","Aboveground_Carbon","Agricultural_Productivity","Flood_Mitigation","Pollination","Plant_Productivity")
# Catchment names - linked to filenames
catchnames<-c( "fal", "fowey", "staust","camel")

# Load service stack for whole of cornwall
cornwall.stack<-stack(paste0(dir_in,"stack.tif"))
names(cornwall.stack)<-service_names

# Prepare data for histograms for all cornwall
for (n in names(cornwall.stack)){
  values.all<-getValues(raster(cornwall.stack,n))
  sel.all<-which(!is.na(values.all))
  values.all<-values.all[sel.all]
  numcells<-length(values.all)
  mnx<-min(values.all,na.rm=TRUE)
  mxx<-max(values.all,na.rm=TRUE)
  allh <- hist(values.all,breaks=seq(0,100,10)) 
  if (n==names(cornwall.stack)[1]) data.all <- allh$mid
  freq<-(allh$counts/numcells)*100
  data.all<- cbind(data.all,freq)
}
colnames(data.all)<-c("Value",service_names)

######################
# Postcode data
######################
cornwall_postcodes.sf<-st_read(paste0(dir_in,"cornwall_postcodes_3857/cornwall_postcodes_3857.shp"))
cornwall.centroids.sf<-st_read(paste0(dir_in,"cornwall_pc_ons_info/cornwall_pc_ons_info.shp"))
cornwall_postcodes.r<-raster(paste0(dir_in,"cornwall_postcodes_3857.tif") )


clrs <- rev(brewer.pal(9, "Spectral"))
pal <- colorBin(clrs, domain = c(0,10,20,30,50,70,80,90,100),  bins = 9, pretty=TRUE, na.color="transparent")
#visibility<-1

########################################################################################
# Define UI
########################################################################################
ui <-dashboardPage(
  dashboardHeader(title="Fal App"),
  dashboardSidebar(width=300,
                   sidebarMenu(id="sidebar_tab",
                               menuItem("About", icon=icon("info") ),
                               menuItem("Catchment Service Display ", tabName="esmap", icon=icon("map")),
                               menuItem("Postcode Report", tabName="pcpage",icon=icon("search"))
                   ), # end of sidebar
                   
                   conditionalPanel("input.sidebar_tab =='esmap'", 
                                    h4(style="padding: 10px 10px 0px;","Ecosystem-services"),
                                        selectInput(inputId="catchment",label="Choose catchment:",
                                                    choices=c("Fal"=catchnames[1],
                                                              "Fowey"=catchnames[2],
                                                              "Par - St Austell"=catchnames[3],
                                                              "Camel"=catchnames[4] ) ) ,
                                        selectInput("bgtype",label="Choose background tiles", 
                                                choices=  c("ESRI satellite world imagery" = "Esri.WorldImagery","OpenStreet map" = "OpenStreetMap.Mapnik"), 
                                                selected="OpenStreetMap.Mapnik",selectize=TRUE  ) ,
                                        sliderInput("visibility","Set transparency:",min=0,max=1,value=0.7) 
                                        
                                      ), # esmap
                   
                   conditionalPanel("input.sidebar_tab =='pcpage'", 
                                    h4(style="padding: 10px 10px 0px;","Enter postcode to generate report:"),
                                        textInput("pc", "Enter postcode:", value = "TR1 6AZ", width = NULL, placeholder = NULL) ,
                                        actionButton("createreport", "Create Report", icon("paper-plane"), width="100%",
                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4")          
                                      ), # pcpage
                   
                   conditionalPanel("input.sidebar_tab =='info'",
                                    h4(style="padding: 10px 10px 0px;","Explanation:")  
                   ) # cond panel - search
  ),
  
  dashboardBody(
    # Java function
    #tags$head(tags$script(HTML(getInputwithJS))),
    
    tabItems(
      tabItem(tabName="esmap",
              fluidRow(height=8,
                       box(width=10 ,
                           textOutput("txt")
                       )
              ),
              
              fluidRow( height=260,
                        box(width=10,
                            div(id = "yrmap-container",
                               
                                leafletOutput("map")
                            )                       
                        ) , # box
                        
                        box(title="Map information", width=2,
                            # htmlOutput("mapmessage"),
                            radioButtons(inputId="service", label="Choose service:", 
                                               c("Biodiversity" = service_names[1],
                                                 "Aboveground Carbon" = service_names[2] ,
                                                 "Agricultural Productivity" = service_names[3],
                                                 "Flood Mitigation" = service_names[4] ,
                                                 "Pollination" = service_names[5] ,
                                                 "Plant Productivity" = service_names[6] ) )   ,
                            checkboxInput("legend", "Show legend", FALSE) 

                            # , textOutput("maptext")
                        )  # box"
              ) , # fluidRow
              
              shinyjs::hidden(
                div(id = "cell",
                    fluidRow(height=75,
                             box(title="Distribution of map values (cell value in red)", width=5,
                                 div(id = "hist-container",
                                     plotOutput("twohist",height="200px")    )
                             ),
                             box(title="Historic variation of cell values", width=5,
                                 div(id = "ts-container"

                                     # OUTPUT plotOutput("timeseriesplot",height="200px")
                                     ) 
                             ),
                             box(title="Cell Selected:", width=2,
                                 htmlOutput("celltext") 
                             )
                    )# fluidRow
                ) 
              )
      ), # tabItem explore
      
      tabItem(tabName="pcmap",
              fluidRow(height=8,
              height=260,
              box(width=10,
                  div(id = "pcmap-container",
                      leafletOutput("pcmap")
                  )                       
              ) , # box
              
              box(title="Area information", width=2,
                  # htmlOutput("mapmessage"),
                  radioButtons(inputId="pcservice", label="Choose service:", 
                               c("Biodiversity" = service_names[1],
                                 "Aboveground Carbon" = service_names[2] ,
                                 "Agricultural Productivity" = service_names[3],
                                 "Flood Mitigation" = service_names[4] ,
                                 "Pollination" = service_names[5] ,
                                 "Plant Productivity" = service_names[6] ) )   ,
                  checkboxInput("legendpc", "Show legend", FALSE) 
                  # , textOutput("maptext")
              )  # box"
      ) 
            
      ) # tabItem
      
    ) # tabItems
  ) # dashboardBody
) # dashboardPage              
              
              
              
              
              
####################################################################################################################################
# Server function
####################################################################################################################################
# Go to postcode function - centres map and overlays postcode polygon - allow click
# Overlay two dif coloured histograms for a service - one catchment/pc one all cornwall

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ###############
  # Reactive functions
  #############
  catchment_stack <- reactive({
    req(input$catchment)
    #js$showid("map-spinner")  # Displays loading spinner - hidden in Observe that displays raster
    r<-stack(paste0(dir_in,input$catchment,"stack.tif"))
    names(r)<-service_names
    crs(r)<-crs('+init=EPSG:3857')
    return(r)
  }) 
  
  ##############################
  # Explore map page
  ############################
   
   output$map<-renderLeaflet({
     leaflet() %>% 
       setView(lng = -4.8, lat = 50.5, zoom = 8)  %>%
       addProviderTiles(input$bgtype)%>%
       addScaleBar() 
   })
   
   # Observe to change map data if UI changes
   observe({
     #priority=1
     #r.stack<-stack(paste0(dir_in,input$catchment,"stack.tif"))
     #names(r.stack)<-service_names
     # Add raster layer chosen
     leafletProxy("map") %>% 
       clearImages() %>% 
       addRasterImage(x=(raster(catchment_stack(),input$service)),
                      colors=pal,
                      opacity=input$visibility, 
                      project=FALSE) 
     
     # Show overlapping histograms
     output$twohist<-renderPlot( twohist(getValues(raster(catchment_stack(),input$service)),
                                         data.all[,input$service] ) 
                                 #+ geom_vline(xintercept = values.df[cellnumber,2],color = "red", size=1) 
                                 )
     })
   
   # Observe to change legend if requested
   observe({
     proxy <- leafletProxy("map")
     # Remove any existing legend, and only if the legend is
     # enabled, create a new one.
     proxy %>% clearControls()
     if (input$legend){
       proxy %>% addLegend(values=values(raster(r,input$service)),pal=pal,position = "bottomright",na.label="Sea" )
     }
   })  
   
# Print title of map/data displayed
   output$txt <- renderText({
     paste("Map of",input$service,"ecosystem service")
   })
   
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   ##############################
   # POSTCODE map page
   ############################
   
   output$pcmap<-renderLeaflet({
     leaflet() %>% 
       setView(lng = -4.8, lat = 50.5, zoom = 8)  %>%
       addProviderTiles(input$bgtype)%>%
       addScaleBar() 
   })
   
   # Actions on when createreport clicked
   observeEvent(input$createreport,{
     # Check valid Postcode
     # Find postcode centroid  lat/lon
     lat<-cornwall.centroid[cornwall.centroid$psd==input$pc,"lat"]
     lng<-cornwall.centroid[cornwall.centroid$psd==input$pc,"long"]
     
     # Create mask around centroid
     
     
     # Plot raster map
     leafletProxy("search_map") %>% 
       setView(lng = lng, lat = lat, zoom = 5)  %>%
       clearPopups() %>%
       clearImages() %>% 
       addRasterImage(raster(cornwall.stack,input$pcservice), 
                      colors=pal,
                      opacity=input$visibility, 
                      project=FALSE) 
     
     # Plot postcode outline
     
     
     # Add centroid marker
     
     
   })
   
   
   # Observe to change legend if requested
   observe({
     proxy <- leafletProxy("pcmap")
     # Remove any existing legend, and only if the legend is
     # enabled, create a new one.
     proxy %>% clearControls()
     if (input$legend){
       proxy %>% addLegend(values=values(raster(r,input$service)),pal=pal,position = "bottomright",na.label="Sea" )
     }
   })  
   
} # server

# Run the application 
shinyApp(ui = ui, server = server)

