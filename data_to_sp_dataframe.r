# getPackage() first looks for a package and loads it, and if it's not on the system will install and load it.
getPackage <- function(pkg){
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
  return(TRUE)
}
# install/load necessary packages
getPackage('jsonlite')
getPackage('sp')
getPackage('rgdal')
getPackage('raster')

# collect species occurrence information from BISON
jsonBISON <- fromJSON("http://bisonapi.usgs.ornl.gov/solr/occurrences/select/?q=ITISscientificName:%22Hylocichla%20mustelina%22&start=0&wt=json&rows=10000") # note the final parameter "rows" determines number of records returned
# extract data from JSON result
json_data <- jsonBISON$response$docs
# remove records that are centroid record
json_data <- json_data[is.na(json_data$centroid) == TRUE,]
# remove records that are of type= 'specimen'
json_data <- json_data[json_data$basisOfRecord != 'specimen',]
# drop records with no coordinates
json_data <- json_data[is.na(json_data$decimalLatitude) == FALSE,]
# identify lat/long fields in dataset
coords <- json_data[,c(9,20)]
# convert data.frame into spatial data frame
sp_data <- SpatialPointsDataFrame(coords,proj4string=CRS("+proj=longlat +datum=WGS84"), data = json_data)
# load US-State-Boundary polygon shapefile
State.Boundary.US <- readOGR('data/NEON-DS-Site-Layout-Files/US-Boundary-Layers',
                             'US-State-Boundaries-Census-2014')
# optional crop US boundary to extent of occurrence data
#US_crop <- crop(State.Boundary.US,extent(sp_data))

# Load in species range polygon shapefile
# anticipated migratory range of wood thrush downloaded from gapanalysis.usgs.gov
thrush_range <- readOGR('data',
                        'WoodThrushRangeSum')
#transform coordinate system to match base layer 
thrush_range_WGS84 <- spTransform(thrush_range,
                                     crs(State.Boundary.US))
# plot US State Boundaries
plot(State.Boundary.US,
     main = 'BISON Occurrence Data \n with GAP Known Range',
     col = 'tan')
# add species range distribution polygon
plot(sim,
     col = 'blue',
     border = FALSE,
     add = TRUE)
# add species occurrence information
plot(sp_data,
     pch = 19,
     cex = 1.0,
     col = 'blue',
     add = TRUE)
plot <- recordPlot()
# Dissolve ajoining polygons into a single large polygon
# reduces 63Mb file to approximately 1.3Mb
# renders much faster
sim <- aggregate(thrush_range_WGS84)
rm(sim)
