##########################
#Communicating to API
# Ref: https://www.r-bloggers.com/accessing-apis-from-r-and-a-little-r-programming/
# https://www.programmableweb.com/news/how-to-access-any-restful-api-using-r-language/how-to/2017/07/21
# https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html
# Status codes ref: https://en.wikipedia.org/wiki/List_of_HTTP_status_codes

# Video: https://www.rstudio.com/resources/videos/using-web-apis-from-r/
# NOMIS: https://www.nomisweb.co.uk/api/v01/help 
##########################

library(jsonlite)
library(httr)
library(sf)
library(geojsonR)
library(geojsonio)

options(stringsAsFactors = FALSE)

##################
# NOMIS api
# Tables: https://www.nomisweb.co.uk/census/2011/key_statistics_uk
# https://www.nomisweb.co.uk/census/2011/local_characteristics

# SOA population estimates: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates


##################
url<-"http://www.nomisweb.co.uk"

# find datasets
path<-"/api/v01/dataset/def.htm?search=*population*"

print(paste0(url,path))

Postcode Population: https://www.nomisweb.co.uk/census/2011/postcode_headcounts_and_household_estimates 
id	NM_136_1
Name	annual population survey - households by combined economic activity status
id	NM_144_1
Name	KS101EW - Usual resident population as of Census 2011 - soa level and above
Name KS102 = age structure
Name KS601 - economic activity

id	NM_143_1
Name	QS102EW - Population density
id	NM_168_1
Name	annual population survey - regional - employment by occupation
id	NM_920_1
Name	ST701EWla - method of travel to work


path<-"/api/v01/dataset/KS101EW/"
subpath<-"def.htm"

paste0(url,path,subpath)

# England codeliest: 2092957699

# List Op catchment by ID
OpID<-3184
wb<-"GB108048001270"
  
#########
#
#########


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

  
  