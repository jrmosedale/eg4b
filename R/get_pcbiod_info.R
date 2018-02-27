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

dir_in<-"/Users/jm622/Data/"
aoi_filename<-"/Users/jm622/Rprojects/landcover/masks/Cornwall.shp"

aonb<-st_read(paste0(dir_in,"AONB_Boundary/Cornwall_AONB.shp"))
sssi<-st_read(paste0(dir_in,"Sites_of_Special_Scientific_Interest_England/nat_floodzone3_v201711.shp"))
fwa.uk<-st_read(paste0(dir_in,"Flood_Warning_Areas/Flood_Warning_PSF_ENG_20170711.shp"))

aoi<-st_read(aoi_filename)


# Template for overlap ie Pcode polygon or circular distance from location


# Inside or distance to SSSI, Local N, AONB, etc within, within 1km, within 5km, 


# Habitat mix of area - incl any priority habitats


# 
<-st_intersection(aonb,aoi)
