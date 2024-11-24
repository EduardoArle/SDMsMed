################################################################################
#                         Preparing GBIF data                                #
################################################################################

#load packages
library(sf)

#list WDs
wd_GBIF <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Uri Roll/Data/GBIF'
wd_variables <- ''

#load GBIF occurrence table
setwd(wd_GBIF)
GBIF <- read.csv('occurrence.txt', sep = '\t')

#eliminate entries without coordinates
GBIF <- GBIF[complete.cases(GBIF$decimalLongitude),]
GBIF <- GBIF[complete.cases(GBIF$decimalLatitude),]

#make spataial object
GBIF_sf <- st_as_sf(GBIF, coords = c('decimalLongitude', 'decimalLatitude'))

#load one of the variables to overlap the occurrences


plot(st_geometry(GBIF_sf))












