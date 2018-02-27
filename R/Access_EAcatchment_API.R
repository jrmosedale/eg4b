##########################
#Communicating to API
# Ref: https://www.r-bloggers.com/accessing-apis-from-r-and-a-little-r-programming/
# https://www.programmableweb.com/news/how-to-access-any-restful-api-using-r-language/how-to/2017/07/21
# https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html
# Status codes ref: https://en.wikipedia.org/wiki/List_of_HTTP_status_codes

# Video: https://www.rstudio.com/resources/videos/using-web-apis-from-r/
#eg Env Agency cachment data
##########################

library(jsonlite)
library(httr)
library(sf)
library(geojsonR)
library(geojsonio)

options(stringsAsFactors = FALSE)

# Define url and path of API
url  <- "http://api.epdb.eu"
path <- "eurlex/directory_code"


# Ex. Get basic information
raw.result <- GET(url = url, path = path)
names(raw.result)
raw.result$status_code

# Explore content
this.raw.content <- rawToChar(raw.result$content)
this.content <- fromJSON(this.raw.content)
class(this.content)
this.content[[1]]

# Turn into dataframe
this.content.df <- do.call(what = "rbind",
                           args = lapply(this.content, as.data.frame))




makeQuery <- function(classifier) {
  this.query <- list(classifier)
  names(this.query) <- "dc"
  return(this.query)
}
##################
# Environmant agency catchment api
# Ref: http://environment.data.gov.uk/catchment-planning/ui/reference
##################
url<-"http://environment.data.gov.uk"
path<-"catchment-planning"

# list river mngmt districts
parameters<-""
qpath<-paste0(path,"/so/RiverBasinDistrict","?",parameters)

# List Op catchment by ID
OpID<-3184
wb<-"GB108048001270"
qpath<-paste0(path,"/so/WaterBody/",wb,".json")

# Get .json format response and convert - could make functiojn of this
get_result <- GET(url = url, path = qpath)
get_result$status_code
res.text<-content(get_result,"text")
#res.parsed<-content(get_result,as="parsed")
json.content <- fromJSON(res.text)
#json.content <- fromJSON(res.text, flatten = TRUE)
res.items <- json.content$items
names(res.items)

res.items$waterbodyNotation
class(res.items$characteristic) # list
res.items$characteristic[[1]]

res.df<-as.data.frame(res.texts_json)
names(res.df)
# Pull out particular characteristics
d.str<-res.df$items.currentVersion.downStream
d.str.df<-d.str[[1]]
dim(d.str.df)
d.strID<-d.str.df[1,]

# Find op catchment
op.id<-res.df$items.inOperationalCatchment..id # gives text array
op.id[1]

# Find 
type.id<-res.df$items.currentVersion.type[[1]]
protect.id<-res.df$items.currentVersion.protectedArea..id[1]
res.df$

  
#########
# Get all waterbodies ID in management catchment 
# {root}/so/ManagementCatchment/{id}/water-bodies
#########

# get_waterbodies<-function()

url<-"http://environment.data.gov.uk"
path<-"catchment-planning"
# Parameters
MnID<-list(3113,3089,3061)
qpath<-paste0(path,"/so/ManagementCatchment/",MnID[[1]],"/water-bodies.json")
# Get .json format response and convert - could make functiojn of this
res <- GET(url = url, path = qpath)
res$status_code
# Get content
res.text<-content(get_result,"text")
json.content <- fromJSON(res)
res.items <- json.content$items
waterbodies<-res.items$waterBodyNotation

names(res.items)[1]<-"id"

###############################################################
# Function get all waterbodies for list of MANAGEMENT catchment areas
# input = list of management catchment IDs
# Accesses Env Agency Catchment API
# Outputs: dataframe of WB identities
# eg for conrwall mcatch<-list(3113,3089,3061)
###############################################################
get_mcatch_wb <- function(mcatch) {
  # results<-get_mcatch_wb(MnID)
  # mcatch<-list(3113,3089,3061)
  url<-"http://environment.data.gov.uk"
  for (n in 1:length(mcatch)){
    path<-paste0("catchment-planning/so/ManagementCatchment/",mcatch[[n]],"/water-bodies.json")
    res <- GET(url = url, path = path)
    # Check for some errors
    if (res$status_code!=200) stop("Status code NOT = 200", call. = FALSE)
    if (http_type(res) != "application/json") stop("API did not return json", call. = FALSE)
    # Convert json response to dataframe 
    res.text<-content(res,"text")
    res.json <- fromJSON(res.text)
    res.df<-res.json$items
    res.df<-flatten(res.df)  # to remove variables that are dataframes - converts to lists
    names(res.df)[1]<-"id" # change to valid names from @id
    if (n==1) results<-res.df else results<-rbind(results,res.df)
  }
  # Return LIST of WB IDs in Management Catchment
  #return(as.list(results$waterBodyNotation)) 
  # Return full dataframe
  return(results)
}

###############################################################
# Function get all waterbodies for list of OPERATIONAL catchment areas
# input = list of management catchment IDs
# Accesses Env Agency Catchment API
# Outputs: dataframe of WB identities
###############################################################
get_opcatch_wb <- function(opcatch) {
  # results<-get_opcatch_wb(OpID)
  # opcatch<-list(3094,3352,3435)
  url<-"http://environment.data.gov.uk"
  for (n in 1:length(opcatch)){
    path<-paste0("catchment-planning/so/OperationalCatchment/",opcatch[[n]],"/water-bodies.json")
    res <- GET(url = url, path = path)
    # Check for some errors
    if (res$status_code!=200) stop("Status code NOT = 200", call. = FALSE)
    if (http_type(res) != "application/json") stop("API did not return json", call. = FALSE)
    # Convert json response to dataframe 
    res.text<-content(res,"text")
    res.json <- fromJSON(res.text)
    res.df<-res.json$items
    res.df<-flatten(res.df)  # to remove variables that are dataframes - converts to lists
    names(res.df)[1]<-"id" # change to valid names from @id
    if (n==1) results<-res.df else results<-rbind(results,res.df)
  }
  # Return LIST of WB IDs in OPERATIONAL Catchment - for direct use as paramter to get Shapes
  # return(as.list(results$waterBodyNotation))
  # Return full dataframe
  return(results)
}

###############################################################
# Function get shape of waterbody
# Input = list of WB ids
# Accesses Env Agency Catchment API
# Outputs: sf database of polygon for each river catchment in list
###############################################################
get_wb_polygons <- function(WBlist) {
  # WBlist<-as.list(unique(results$waterBodyNotation))[1:3]
  url<-"http://environment.data.gov.uk"
  for (n in 1:length(WBlist)){
    path<-paste0("catchment-planning/so/WaterBody/",WBlist[[n]],"/polygon")
    get_shape <- GET(url = url, path = path)
    # Check for some errors
    if (get_shape$status_code!=200) stop("Status code NOT = 200", call. = FALSE)
    if (http_type(get_shape) != "application/geo+json") stop("API did not return json", call. = FALSE)
    shape.text<-content(get_shape,"text")
    shape.sf<-st_read(shape.text)  
    if (n==1) results.sf<-shape.sf else results.sf<-rbind(results.sf,shape.sf)
  }
  # plot(results.sf$geometry)
  results.sf<-cbind(results.sf,unlist(WBlist))
  return(results.sf)
}

###############################################################
# Function Shape of Operational Catchment (includes coastal areas)
# Input = list of WB ids
# Accesses Env Agency Catchment API
# Outputs: sf database of polygon for each Operational catchment in list
###############################################################
get_op_polygons <- function(oplist) {
  # WBlist<-as.list(unique(results$waterBodyNotation))[1:3]
  url<-"http://environment.data.gov.uk"
  for (n in 1:length(oplist)){
    path<-paste0("catchment-planning/so/OperationalCatchment/",oplist[[n]],"/polygon")
    get_shape <- GET(url = url, path = path)
    # Check for some errors
    if (get_shape$status_code!=200) stop("Status code NOT = 200", call. = FALSE)
    if (http_type(get_shape) != "application/vnd.geo+json") stop("API did not return geojson", call. = FALSE)
    shape.text<-content(get_shape,"text")
    shape.sf<-st_read(shape.text)  
    if (n==1) results.sf<-shape.sf else results.sf<-rbind(results.sf,shape.sf)
  }
  # plot(results.sf$geometry)
  results.sf<-cbind(results.sf,unlist(oplist))
  return(results.sf)
}

###############################################################
# FUNCTION Gets an array of protected area types from a list of protected area IDs
# Input: array of protected area ID
# Output: array of corresponding protected types (nitrate-directive etc)
###############################################################
get_protectarea_type<-function(protect.areas){
  for (n in 1:length(protect.areas)){
    protect<- GET(url = paste0(protect.areas[n],".json") )
    # Check for some errors
    if (protect$status_code!=200) stop("Status code NOT = 200", call. = FALSE)
    if (http_type(protect) != "application/json") stop("API did not return json", call. = FALSE)
    protect.text<-content(protect,"text")
    protect.json <- fromJSON(protect.text,flatten=TRUE)
    protect.df<-protect.json$items
    protect.df<-flatten(protect.df, recursive=TRUE)  # to remove variables that are dataframes - converts to lists
    names(protect.df)[1]<-"id" # change to valid names from @id
    names(protect.df)
    zone<-unlist(strsplit(protect.df$'protectedAreaType.@id',"/"))
    zone<-zone[length(zone)] # Gives directive
    if (n==1) result<-zone else result<-c(result,zone)
  }
  print(result)
  return(result)
} # end function get_protectarea_type


###############################################################
# FUNCTION to get Classification Info for a wb
# Input: waterbidy id
# Output: data.frame of MOST RECENT year classification data
###############################################################
get_classf_info<-function(wb){
  # Get classification info
  url<-"http://environment.data.gov.uk"
  path<-paste0("catchment-planning/so/WaterBody/",wb,"/classifications.json")
  print(path)
  clasf <- GET(url = url, path = path)
  # Check for some errors
  if (clasf$status_code!=200) stop("Status code NOT = 200", call. = FALSE)
  if (http_type(clasf) != "application/json") stop("API did not return json", call. = FALSE)
  clasf.text<-content(clasf,"text")
  clasf.json <- fromJSON(clasf.text,flatten=TRUE)
  clasf.df<-clasf.json$items
  
  # Extract variables of use and assign to features/values
  df<-data.frame(clasf.df$classificationItem.label,clasf.df$classificationValue.label,clasf.df$classificationYear,clasf.df$cycle.notation)
  colnames(df)<-c("Feature","Value","Year","Cycle")
  
  # Choose most recent data ONLY
  # See: https://stackoverflow.com/questions/35517280/select-rows-by-most-recent-year
  df.by<-by(df, df$Value, FUN = function(x) x[x$Year == max(x$Year),])
  df <- Reduce(function(...) merge(..., all=T), df.by)
  
  return(df)
}


###############################################################
# FUNCTION Get characteristics of single river catchment
# Input: waterbidy id
# Output dataframe of features and values/text
# Uses function: get_protectarea_type, get_classf_info
###############################################################
get_wb_info <- function(wb) {
  # Get information via json to dataframe
  url<-"http://environment.data.gov.uk"
  path<-paste0("catchment-planning/so/WaterBody/",wb,".json")
  print(paste("get_wb_info",path) )
  res <- GET(url = url, path = path)
  # Check for some errors
  if (res$status_code!=200) stop("Status code NOT = 200", call. = FALSE)
  if (http_type(res) != "application/json") stop("API did not return json", call. = FALSE)
  # Convert json response to dataframe 
  res.text<-content(res,"text")
  res.json <- fromJSON(res.text,flatten=TRUE)
  res.df<-res.json$items
  res.df<-flatten(res.df, recursive=TRUE)  # to remove variables that are dataframes - converts to lists
  names(res.df)[1]<-"id" # change to valid names from @id
  names(res.df)
  
  # IMPORTANT: see https://stackoverflow.com/questions/32986753/extracting-elements-from-a-jsonlite-parsed-list-in-r
  # Select data and collect new results dataframe
  features<-c("Name","Type","Description","Area","Length",
              "Overall_Quality","Ecology_Quality","Chemical_Quality","Bad results","High levels",
              "Protection", "Link")
  values<-rep("",length(features))
  names(values)<-features
  results<-data.frame(features,values)
  
  # Type, Name, area length
  values["Area"]<-res.df$characteristic[[1]]$characteristicValue[1] # in ha
  values["Length"]<-res.df$characteristic[[1]]$characteristicValue[2] # in km
  values["Name"]<-res.df$currentVersion.label
  # Hydromorphological designation 
  values["Description"]<-res.df$currentVersion.hydromorphologicalDesignation.label
  
  # Areas of Protection: requires further queries
  protect.areas<-unlist(res.df$currentVersion.protectedArea) # create array of area links
  protect.types<-get_protectarea_type(protect.areas) # get types of area
  # Merge results to single element HERE !!!!
  values["Protection"]<-paste(unique(protect.types), collapse=",")
  
  # HTTP link to HTML page summarising catchment
  # format: http://environment.data.gov.uk/catchment-planning/WaterBody/GB108048001150  
  values["Link"]<-paste0("http://environment.data.gov.uk/catchment-planning/WaterBody/",wb)
  
  # Get Classifiction data via API 
  df<-get_classf_info(wb)
  
  # Assign Overall waterbody, Ecolog, Chemical
  values["Ecology_Quality"]<-df$Value[which(df$Feature=="Ecological")]
  values["Chemical_Quality"]<-df$Value[which(df$Feature=="Chemical")]
  values["Overall_Quality"]<-df$Value[which(df$Feature=="Overall Water Body")]
  
  # Assign characteristics with bad / high levels
  bad<-c("Fail","Bad","Poor")
  values["Bad results"]<-""
  if (any(df$Value %in% bad)) {
    values["Bad results"]<-paste(df[which(df$Value %in% bad),] ,collapse=", ")
  }
  highbad<-c("Zinc","Phosphate","Copper","Ammonia",
             "Specific pollutants","Ammonia (Phys-Chem)","Temperature") 
  values["High levels"]<-""
  
  if(any(df$Value %in% "High" & df$Feature %in% highbad)) {
    high.df<-df[which(df$Value %in% "High" & df$Feature %in% highbad),] 
    values["High levels"]<-paste(high.df$Feature,collapse=", ")
  }
  
  result.table<-data.frame(cbind(features,values))
  return(result.table)
} # end function get_wb_info



result<-get_wb_info(wb)


#### USE API Catchment Functions - EXAMPLES

# Get spatial data for all polygons in Cornwall
# Get WB IDs for all Cornwall
mcatch<-list(3113,3089,3061)
cornwall_wb<-get_mcatch_wb(mcatch)

# Get shape files for all operational catchments in Cornwall
cornwall_ops<-unique(cornwall_wb$`isVersionOf.inOperationalCatchment.@id`)
oplist<-as.list(substr(cornwall_ops,nchar(cornwall_ops)-3,nchar(cornwall_ops)))
cornwall_op.sf<-get_op_polygons(oplist)

names(cornwall_op.sf)<-c("OpID","geometry")
plot(cornwall_op.sf$geometry)

# Get id for Fal catchment
sel<-which(cornwall_wb$isVersionOf.inOperationalCatchment.label=="Fal")
opid.string<-unique(cornwall_wb$`isVersionOf.inOperationalCatchment.@id`[sel])
if (length(opid)>1) warning("multiple opid for op catchment")
opid<-substr(opid.string,nchar(opid.string[1])-3,nchar(opid.string) ) # assumes op id always 4 characters

# Get shape for Fal
falop.sf<-get_op_polygons(opid)
sel<-which(cornwall_op.sf$OpID==opid)
falop.sf<-cornwall_op.sf[sel,] # get shape file for fal catchment


# Get WB IDs for Fal catchment
fal_wb<-get_opcatch_wb(opid)
print(fal_wb$waterBodyNotation)

# Get and plot polygons for Fal catchment
WBlist<-fal_wb$waterBodyNotation
fal.sf<-get_wb_polygons(WBlist)
plot(falop.sf$geometry,col="red")
plot(fal.sf$geometry,add=TRUE)






#########







  
  
  
  
  # Try to convert directly to sf
  get_shape <- GET(url = url, path = qpath)
  # Check for some errors
  if (get_shape$status_code!=200) stop("Status code NOT = 200", call. = FALSE)
  if (http_type(get_shape) != "application/json") stop("API did not return json", call. = FALSE)
  shape.text<-content(get_shape,"text")
  
  test.sf<-st_read(shape.text)  
  
  shape_json <- fromJSON(shape.text, flatten = TRUE)
  shape<-shape_json$items
  coords<-shape_json[[2]]
  # Pageing of data = common - sometimes nex previous links
  
  