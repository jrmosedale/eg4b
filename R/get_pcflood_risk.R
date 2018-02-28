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
library(units)

dir_ea<-"/Volumes/Pocket_Sam/Data/EnvAgency/"
aoi_filename<-"/Users/jonathanmosedale/Rprojects/eg4b/Masks/Cornwall.shp"

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
aoi_filename<-"/Users/jonathanmosedale/Rprojects/eg4b/Masks/Cornwall.shp"

# Load data and set crs
fzone2<-st_read(paste0(dir_ea,"Flood_zone_2/cornwall_floodzone2_v201711.shp"))
fzone3<-st_read(paste0(dir_ea,"Flood_zone_3/cornwall_floodzone3_v201711.shp"))
fwa<-st_read(paste0(dir_ea,"Flood_Warning_Areas/cornwall_floodwarning201711.shp"))
st_crs(fzone2)<-st_crs(27700)
st_crs(fzone3)<-st_crs(27700)
st_crs(fwa)<-st_crs(27700)

# Useful Functions
# Return TRUE / FALSE according to whetrher point inside any polygon
in_polygons <- function(polygons, point) {
  result<-st_intersects(polygons,point)
  final<- (1 %in% result) 
  #polygon.sf<-polygons[final,]
  return(final)
}

in_which_polygon <- function(polygons, point) {
  polygons<-st_union(polygons)
  overlap<-st_intersects(polygons,point)
  print (paste("Number of polygins in which point falls=",length(which(overlap==1))))
  if (1 %in% overlap) result<-overlap[which(overlap==1)] else result<-0
  print(result)
  return(result)
}

distance_to_polygon<-function(polygons, point) {
  #if (length(polygons)>1) polygons<-st_union(polygons)
  if (1 %in% st_intersects(polygons,point)) distance<-0 else distance<-st_distance(point,polygons)
  print(paste("Distance to polygon=",distance))
  return(distance)
}


######  Test points inside and outside of FWA polygons
plot(fwa$geometry[10:11])
pos1<-st_sfc(st_point(c(183100,44500))) # edge outside
pos2<-st_sfc(st_point(c(183900,44400))) # outside
pos3<-st_sfc(st_point(c(183049,44080))) # inside
st_crs(pos1)<-st_crs(27700)
st_crs(pos2)<-st_crs(27700)
st_crs(pos3)<-st_crs(27700)

plot(pos1,add=TRUE,col="red")
plot(pos2,add=TRUE,col="blue")
plot(pos3,add=TRUE,col="green")

plot(fwa$geometry[11],add=TRUE,col="yellow")

# Calculate distance from point to nearest polygon
# Ensure same crs and units of crs are appropriate ie metres/km etc
# Simplify geometries?
fwa_union<-st_union(fwa)
fzone2<-st_simplify(fzone2)
fzone2<-st_union(fzone2)
fzone3<-st_union(st_simplify(fzone3))

# Call function to calculate distance - 0=point within a polygon
fwadist<-distance_to_polygon(fwa,pos1)
fz2_dist<-distance_to_polygon(fzone2,pos1)
fz3_dist<-distance_to_polygon(fzone3,pos1)

# Identify if within a polygon
in_polygons(pos2,fwa)

if (in_polygons(pos2,fwa)==TRUE) 
  
  
# Alternative code
  
min_distance<-min(st_distance(pos, fzone3$geometry,by_element = TRUE))
nearest_polygon<-which(st_distance(pos, fzone3$geometry,by_element = TRUE)==min(st_distance(pos, fzone3$geometry,by_element = TRUE)))

print(min_distance)  
print(nearest_polygon)
print(fzone3[nearest_polygon,])

