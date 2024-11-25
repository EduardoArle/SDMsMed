################################################################################
#                         Preparing GBIF data                                  #
################################################################################

#load packages
library(sf); library(terra)

#list WDs
wd_GBIF <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Uri Roll/Data/GBIF'
wd_variables <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Uri Roll/Data/BioOracle/Present'

#load GBIF occurrence table
setwd(wd_GBIF)
GBIF <- read.csv('occurrence.txt', sep = '\t')

#eliminate entries without coordinates
GBIF <- GBIF[complete.cases(GBIF$decimalLongitude),]
GBIF <- GBIF[complete.cases(GBIF$decimalLatitude),]

#make spataial object
GBIF_sf <- st_as_sf(GBIF, coords = c('decimalLongitude', 'decimalLatitude'))

#load variable layers
setwd(wd_variables)
vars <- lapply(list.files(pattern = '.tif$'), rast)

#create an ID raster from one of the variables
ID_raster <- var
ID_raster[which(!is.na(var[[1]][]))] <- c(1:length(which(!is.na(var[[1]][]))))














