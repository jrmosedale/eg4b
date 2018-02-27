library(flexdashboard)
library(shiny)
library(raster)
library(leaflet)
#library(DT)
library(magrittr)
library(colorspace)
library(rgeos)
library(ggplot2)
#library(shinyjs)
#library(plotly)
library(DiagrammeR)
library(rgdal)
library(sf)
#library(tidyverse)
library(leaflet.extras)

dir_ea<-"/Users/jm622/Data/EnvAgency/"
aoi_filename<-"/Users/jm622/Rprojects/landcover/masks/Cornwall.shp"

fzone2.uk<-st_read(paste0(dir_ea,"Flood_zone_2/nat_floodzone2_v201711.shp"))
fzone3.uk<-st_read(paste0(dir_ea,"Flood_zone_3/nat_floodzone3_v201711.shp"))
fwa.uk<-st_read(paste0(dir_ea,"Flood_Warning_Areas/Flood_Warning_PSF_ENG_20170711.shp"))


aoi<-st_read(aoi_filename)
st_crs(fzone2.uk)<-st_crs(27700)
st_crs(fzone3.uk)<-st_crs(27700)
st_crs(fwa.uk)<-st_crs(27700)

st_crs(aoi)<-st_crs(27700)
fzone2.uk<-st_intersection(fzone2.uk,aoi)
fzone3.uk<-st_intersection(fzone3.uk,aoi)
fwa.uk<-st_intersection(fwa.uk,aoi)

st_write(fzone2.uk,paste0(dir_ea,"Flood_zone_2/cornwall_floodzone2_v201711.shp"))
st_write(fzone3.uk,paste0(dir_ea,"Flood_zone_3/cornwall_floodzone3_v201711.shp"))
st_write(fwa.uk,paste0(dir_ea,"Flood_Warning_Areas/cornwall_floodwarning201711.shp"))

lat<-50.267512 
lon<- -5.032867
pos<-st_point(c(lon,lat))

in_polygons <- function(polygons, point) {
  result<-st_intersects(polygons,point)
  final<- (1 %in% result) 
  #polygon.sf<-polygons[final,]
  return(final)
}

print(in_polygons(fzone3.uk,pos))

#######################################
#Source code to be included in app
#
###########################################
# Load data
fzone2<-st_read(paste0(dir_ea,"Flood_zone_2/cornwall_floodzone2_v201711.shp"))
fzone3<-st_read(paste0(dir_ea,"Flood_zone_3/cornwall_floodzone3_v201711.shp"))
fwa<-st_read(paste0(dir_ea,"Flood_Warning_Areas/cornwall_floodwarning201711.shp"))

# Functions
# Return TRUE / FALSE according to whetrher point inside any polygon
in_polygons <- function(polygons, point) {
  result<-st_intersects(polygons,point)
  final<- (1 %in% result) 
  #polygon.sf<-polygons[final,]
  return(final)
}

# Calculate distance from point to nearest polygon

st_distance(x, y, ..., dist_fun, by_element = FALSE, which = "distance",
            par = 0, tolerance = 0)
