# Load and prepare shape files for plotting in Leaflet
# Requires transformation to crs - st_crs(data)<-"+init=epsg:4326"

################################################
# Directories & online data
###############################################
in.root<-"/Users/jm622/apps/fal/"
dir_in<-paste(in.root,"www/",sep="")
dir_data<-"/Volumes/Pocket Sam/Data/"

fal_shape_4326<-st_read(paste0(dir_rasters,"fal_shape/fal_shape.shp"))

# Dom urban/rural/other - using landcover map
domcover.r<-raster(paste(dir_in,"domcover.tif",sep=""))
habitat_names<-c( "Broadleaf woodland", "Coniferous woodland",
                  "Arable & Horticulture", "Improved grassland",
                  "Neutral grassland", "Calcareous grassland",
                  "Acid grassland", "Fen, Marsh, Swamp",
                  "Heather", "Heather grassland",
                  "Bog", "Inland rock",
                  "Saltwater", "Freshwater",
                  "Supra-littoral rock", "Supra-littoral sediment",
                  "Littoral rock", "Littoral sediment",
                  "Saltmarsh", "Urban",
                  "Suburban"
)

urban.r<-calc(domcover.r,fun=function(x){ifelse(x %in% c(20,21),1,NA)})
plot(urban.r,col="black")
rural.r<-calc(domcover.r,fun=function(x){ifelse(x %in% c(3,4),1,NA)})
plot(rural.r,col="green",add=TRUE)
other.r<-calc(domcover.r,fun=function(x){ifelse(x %in% c(1:2,5:19),1,NA)})
plot(other.r,col="red",add=TRUE)
#plot(domcover.r)


# Load CORNWALL rasters 
template.r<-raster("/Volumes/Pocket Sam/Data/MapApp/Projected_rasters/cornwall_land_100m.tif")

priority.r<-raster(paste(root,"priority_habitat.tif",sep=""))
spa.r<-raster(paste(root,"spa.tif",sep=""))
sac.r<-raster(paste(root,"sac.tif",sep=""))
sssi.r<-raster(paste(root,"sssi.tif",sep=""))
localn.r<-raster(paste(root,"localn.tif",sep=""))

nature.stack<-stack(priority.r,spa.r,sac.r,sssi.r,localn.r)
nature.stack<-projectRaster(nature.stack,template.r)
names(nature.stack)<-c("Priority","SPA","SAC","SSSI","Local")

# Mask to fal
msk<-st_read(paste0(dir_rasters,"fal_shape/fal_shape.shp") )
msk<-st_transform(msk, 3857)
crs(nature.stack)<-crs("+init=epsg:3857")
falnat.stack<-mask(nature.stack,as(msk,'Spatial'))


msk<-as(msk<-st_transform(fal_shape_4326, 3857),'Spatial')
crs(msk)<-crs("+init=epsg:3857")





# Priority habitats
priority.st<-st_read(paste(dir_data,"NaturalEngland/PriorityHabitats_SOuth/PHI_v2_1_South.shp",sep=""), stringsAsFactors = FALSE)
cornwall_priority.st<-st_intersection(priority.st, st_set_crs(st_as_sf(as(extent(cornwall_land.r), "SpatialPolygons")), st_crs(priority.st)))
cornwall_priority.st<-st_transform(cornwall_priority.st,"+init=epsg:4326")
#st_crs(cornwall_priority.st)<-"+init=epsg:4326"
fal_priority.st<-st_intersection(cornwall_priority.st, fal_shape_4326)


# Legend
priority_lookup.df<-read.csv(file = paste0("/Users/jm622/apps/fal/www/","priorityhabitat_names.csv"))


# SOA - deprivation


# SOA - greenspace



# Sites - SSSI, AONB, ...
sssi.st<-st_read(paste(dir_data,"NaturalEngland/Sites_of_Special_Scientific_Interest_England/Sites_of_Special_Scientific_Interest_England.shp",sep=""))
#sssi.st<-st_transform(sssi.st, 27700)
#cornwall_sssi.st<-st_intersection(sssi.st, st_set_crs(st_as_sf(as(extent(cornwall_land.r), "SpatialPolygons")), st_crs(sssi.st)))

fal_sssi.st<-st_intersection(sssi.st,fal_shape_4326)

