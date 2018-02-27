# Process data for Catchment app


library(rgdal)
library(rgeos)
library(raster)
library(ggplot2)
library(sf)

#######################################
##### Directories
#######################################
in.root<-"/Volumes/Pocket Sam/Data/"
#root<-"/Volumes/Pocket Sam/Data/MapApp/Indicator_data/"
dir_rasters<-"/Volumes/Pocket Sam/Data/MapApp/Indicator_data/" # OS Projected data
# dir_rasters<-"/Users/jm622/apps/Map_rasters_100m/"

# Directory for files used by app
dir_out<-"/Users/jm622/apps/fal/www/"
dir_app<-"/Users/jm622/apps/fal/"

##################################
# Load Catchment shapes
# Load both catchment and estuary
# ADD ??? coastal area as well?
#################################
UKcatchments.sf<-st_read(paste(in.root,"EnvAgency/WFD_Surface_Water_Operational_Catchments_Cycle2_gml/WFD_Surface_Water_Operational_Catchments_Cycle2.gml",sep="") )
st_crs(UKcatchments.sf)<-"+init=epsg:27700"
unique(UKcatchments.sf$OPCAT_NAME)
fal.sf<-UKcatchments.sf[UKcatchments.sf$OPCAT_NAME == "Fal"|UKcatchments.sf$OPCAT_NAME == "Carrick Roads",]
fowey.sf<-UKcatchments.sf[UKcatchments.sf$OPCAT_NAME == "Fowey"|UKcatchments.sf$OPCAT_NAME == "Fowey Estuary",]
staust.sf<-UKcatchments.sf[UKcatchments.sf$OPCAT_NAME == "Par, St Austell and Caerhays",]
camel.sf<-UKcatchments.sf[UKcatchments.sf$OPCAT_NAME == "Camel"|UKcatchments.sf$OPCAT_NAME == "Camel Estuary",]

#plot(fowey.sf$geometry) ; plot(fal.sf$geometry,add=TRUE)

# Load river catchments within surface water catchment area 
rivercatch.sf<-st_read(paste(in.root,"EnvAgency/WFD_River_Waterbody_Catchments_Cycle2/WFD_River_Waterbody_Catchments_Cycle2.shp",sep="") )
st_crs(rivercatch.sf)<-"+init=epsg:27700"
unique(rivercatch.sf$WB_NAME)
falrivers.sf<-st_intersection(rivercatch.sf, fal.sf)
foweyrivers.sf<-st_intersection(rivercatch.sf, fowey.sf)
staustrivers.sf<-st_intersection(rivercatch.sf, staust.sf)
camelrivers.sf<-st_intersection(rivercatch.sf, camel.sf)

#plot(foweyrivers.sf$geometry) ; plot(falrivers.sf$geometry,add=TRUE)

# Combine catchment and estuary into single shape
fal_shape<-st_union(fal.sf)
st_crs(fal_shape)<-"+init=epsg:27700"
fowey_shape<-st_union(fowey.sf)
st_crs(fowey_shape)<-"+init=epsg:27700"
camel_shape<-st_union(camel.sf)
st_crs(camel_shape)<-"+init=epsg:27700"

######################################
# Reproject and save Catchment shapes and rasters as EPSG 3857
#####################################

projRforMap<-function(r){
  raster::projectRaster(
    r, raster::projectExtent(r, crs = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")),
    method = "ngb")
}

# Reproject and write shape files
fal_shape_3857<-st_transform(fal_shape, 3857) # Is this required for leaflet???
st_write(fal_shape_3857,dsn=paste(dir_out,"fal_shape",sep=""),layer="fal_shape.shp",driver="ESRI Shapefile")

fowey_shape_3857<-st_transform(fowey_shape, 3857) # Is this required for leaflet???
st_write(fowey_shape_3857,dsn=paste(dir_out,"fowey_shape",sep=""),layer="fowey_shape.shp",driver="ESRI Shapefile")

staust_shape_3857<-st_transform(staust.sf, 3857) # Is this required for leaflet???
st_write(staust_shape_3857,dsn=paste(dir_out,"staust_shape",sep=""),layer="staust_shape.shp",driver="ESRI Shapefile")

camel_shape_3857<-st_transform(camel_shape, 3857) # Is this required for leaflet???
st_write(camel_shape_3857,dsn=paste(dir_out,"camel_shape",sep=""),layer="camel_shape.shp",driver="ESRI Shapefile")

# rasterize,reproject & write - must rasterize on 3857 projected template
template.r<-raster("/Users/jm622/apps/fal/www/cornwall_land_100m.tif")
crs(template.r)<-crs('+init=EPSG:3857')
fal.r<-rasterize(as(fal_shape_3857,"Spatial"),template.r )
writeRaster(fal.r,paste(dir_out,"fal.tif",sep=""),overwrite=TRUE)
fowey.r<-rasterize(as(fowey_shape_3857,"Spatial"),template.r )
writeRaster(fowey.r,paste(dir_out,"fowey.tif",sep=""),overwrite=TRUE)
staust.r<-rasterize(as(staust_shape_3857,"Spatial"),template.r )
writeRaster(fowey.r,paste(dir_out,"staust.tif",sep=""),overwrite=TRUE)
camel.r<-rasterize(as(camel_shape_3857,"Spatial"),template.r )
writeRaster(camel.r,paste(dir_out,"camel.tif",sep=""),overwrite=TRUE)
##################################
# Ecosystem services by catchment
#################################

# Load Ecosysem service rasters, and created masked stack per catchment
r<-brick("/Users/jm622/apps/fal/www/stack.tif")
service_names<-c("Biodiversity","Aboveground_Carbon","Agricultural_Productivity","Flood_Mitigation","Pollination","Plant_Productivity")
names(r)<-service_names
crs(r)<-crs('+init=EPSG:3857')

catchments<-c("fal","fowey","staust","camel")

for (n in 1:length(catchments)){
  msk.r<-raster(paste(dir_rasters,catchments[n],".tif",sep=""))
  crs(msk.r)<-crs('+init=EPSG:3857')
  x<-mask(r,msk.r)
  plot(raster(x,1),main=catchments[n])
  writeRaster(x,filename=paste0(dir_out,catchments[n],"stack.tif"),overwrite=TRUE)
  #assign(paste0(catchments[n], ".stack"),x) # assigns stack to new variable catchmentname.stack
}


####################################
# Postcode data 
####################################
# Load postcode polygons that overlap cornwall shape map

# Load cornwall shape - OS projection
dir_cornwall<-paste(in.root,"Boundaries/geog_areas/",sep="") 
cornwall.sf<-st_read(paste(dir_cornwall,"Cornwall_mainland_outline_OSGB.shp",sep=""))
st_crs(cornwall.sf)<-"+init=epsg:27700"

# Load TR postcodes 
tr<-st_read(paste(in.root,"/Boundaries/codepoint-poly_1912676/two_letter_pc_code/tr/tr.shp",sep=""))
st_crs(tr)<-"+init=epsg:27700"
sel<-st_intersects(cornwall.sf,tr) # includes those fully and partially overlapping
cornwall_tr.sf <- tr[unlist(sel),]

# Load PL postcodes 
pl<-st_read(paste(in.root,"/Boundaries/codepoint-poly_1912676/two_letter_pc_code/pl/pl.shp",sep=""))
st_crs(pl)<-"+init=epsg:27700"
sel<-st_intersects(cornwall.sf,pl) # includes those fully and partially overlapping
cornwall_pl.sf <- pl[unlist(sel),]

# Load EX postcodes 
ex<-st_read(paste(in.root,"/Boundaries/codepoint-poly_1912676/two_letter_pc_code/ex/ex.shp",sep=""))
st_crs(ex)<-"+init=epsg:27700"
sel<-st_intersects(cornwall.sf,ex) # includes those fully and partially overlapping
cornwall_ex.sf <- ex[unlist(sel),]

# Load TQ postcodes 
tq<-st_read(paste(in.root,"/Boundaries/codepoint-poly_1912676/two_letter_pc_code/tq/tq.shp",sep=""))
st_crs(tq)<-"+init=epsg:27700"
sel<-st_intersects(cornwall.sf,tq) # includes those fully and partially overlapping
cornwall_tq.sf <- tq[unlist(sel),]

# Merge to single dataframe of all postcode polygons
cornwall_postcodes.sf<-rbind(cornwall_tr.sf,cornwall_pl.sf,cornwall_ex.sf, cornwall_tq.sf) # polygons of all postcodes within catchment
# Exclude VPL/VTR codes ?????
cornwall_postcodes.sf<-cornwall_postcodes.sf[substr(cornwall_postcodes.sf$POSTCODE,1,1)!="V",]
#plot(st_union(cornwall_postcodes.sf)) # excludes IoS!!!

# ONS postcode data - CENTROIDS and other Boundary IDs
ons.pc.sf<-st_read(paste(in.root,"/ONS/National_Statistics_Postcode_Lookup_Latest_Centroids/National_Statistics_Postcode_Lookup_Latest_Centroids.shp",sep=""))

# Reduce to Pcodes found in Cornwall
cornwall.pcs<-unique(cornwall_postcodes.sf$POSTCODE)
cornwall.centroids<-ons.pc.sf[ons.pc.sf$pcds %in% cornwall.pcs,]  # always use pcds variable

# Reproject and writepolygons
#st_write(cornwall_postcodes.sf,dsn=paste(dir_out,"cornwall_pc_shapes",sep=""),layer="cornwall_pc_shapes",driver="ESRI Shapefile")
cornwall_postcodes_3857<-st_transform(cornwall_postcodes.sf, 3857)
st_write(cornwall_postcodes_3857,dsn=paste(dir_out,"cornwall_postcodes_3857",sep=""),layer="cornwall_postcodes_3857",driver="ESRI Shapefile")

# Write centroids - incl lat/lon data so no need to project
st_write(cornwall.centroids,dsn=paste(dir_out,"cornwall_pc_ons_info",sep=""),layer="cornwall_pc_ons_info",driver="ESRI Shapefile")

# rasterize and write postcode shapes in 3857 projection
template.r<-raster(paste(dir_out,"cornwall_land_100m.tif",sep="")) # crs 3857
cornwall_postcodes_3857.r<-rasterize(as(cornwall_postcodes_3857,"Spatial"),template.r)
plot(cornwall_postcodes_3857.r)
writeRaster(cornwall_postcodes_3857.r,paste0(dir_out,"cornwall_postcodes_3857.tif"),overwrite=TRUE)

####################################



fal.pc.sf<-ons.pc.sf[substr(ons.pc.sf$pcd,1,2) == "TR" | substr(ons.pc.sf$pcd,1,2) == "PL",]
st_write(ons.pc.sf,filename=paste(in.root,"/ONS/National_Statistics_Postcode_Lookup_Latest_Centroids/TR_PL_codes.shp",sep=""))
st_crs(fal.pc.sf)<-"+init=epsg:27700"
sel<-st_intersects(fal.sf,fal.pc.sf)
fal.pc.sf<-fal.pc.sf[unlist(sel),]
plot(fal.pc.sf)




# Plot postcodes and catchment area
plot(fal_pl.sf$geometry,border=1)
plot(fal.sf$geometry,add=TRUE,border=3)

# Rasterize Postcodes
cornwall.r<-raster(paste(dir_rasters,"cornwall_land_100m.tif",sep="")) # crs 27700
fal_pc.r<-rasterize(as(st_union(fal_postcodes.sf),"Spatial"),cornwall.r)
#fal_pc.r<-crop(fal_pc.r,as(fal_shape_5km,'Spatial'))  #  crop to buffered catchment
plot(fal_pc.r) 
