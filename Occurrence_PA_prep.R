################################################################################
#                          Prepare pseudo-absence                              #
################################################################################

#load packages
library(sf); library(terra); library(data.table)

#list WDs
wd_medits <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Uri Roll/Data/MEDITS'
wd_GBIF <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Uri Roll/Data/GBIF'
wd_order <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Uri Roll/Data/Pseudo_absences'
wd_variables <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Uri Roll/Data/BioOracle/Present'


#################### Variables ######################

#load variable layers
setwd(wd_variables)
vars <- lapply(list.files(pattern = '.tif$'), rast)

#create an ID raster from one of the variables
ID_raster <- vars[[1]]
ID_raster[which(!is.na(vars[[1]][]))] <- c(1:length(which(!is.na(vars[[1]][]))))


#################### Pseudo-absence data  ######################


#load GBIF occurrence for all species in the order (Perciformes)
setwd(wd_order)
order <- read.csv('occurrence.txt', sep = '\t')

#eliminate occurrences without coordinates
order <- order[complete.cases(order$decimalLongitude),]
order <- order[complete.cases(order$decimalLatitude),]

#eliminate occurrences of the focus species
order <- order[-which(order$species == 'Epinephelus aeneus'),]

#select 10,000 random occurrences to reduce computational time
order <- order[sample(c(1:nrow(order)), 10000),]

#create object sf for order records
order_sf <- st_as_sf(order, coords = c('decimalLongitude', 'decimalLatitude'))


#################### Presence data  ######################


#load MEDITS fish table
setwd(wd_medits)
fish_table <- read.csv('TBdata_fish.csv')

#eliminate occurrences without coordinates or without species name
fish_table <- fish_table[complete.cases(fish_table$hauling_longitude),]
fish_table <- fish_table[complete.cases(fish_table$hauling_latitude),]
fish_table <- fish_table[complete.cases(fish_table$speciesName),]

#select the focus species
MEDITS <- fish_table[fish_table$speciesName == 'Epinephelus aeneus',]
MEDITS$database <- 'MEDITS'

#load GBIF occurrence table
setwd(wd_GBIF)
GBIF <- read.csv('occurrence.txt', sep = '\t')
GBIF$database <- 'GBIF'

#eliminate entries without coordinates
GBIF <- GBIF[complete.cases(GBIF$decimalLongitude),]
GBIF <- GBIF[complete.cases(GBIF$decimalLatitude),]

#make spataial objects
MEDITS_sf <- st_as_sf(MEDITS, coords = c('hauling_longitude', 'hauling_latitude'))
GBIF_sf <- st_as_sf(GBIF, coords = c('decimalLongitude', 'decimalLatitude'))

#visualise
plot(ID_raster, col = 'azure3', colNA = 'cornsilk1',
     box = NA, legend = NA, axes = NA)
plot(GBIF_sf, add = T, col = 'darkgreen', pch = 19, cex = 0.3)
plot(MEDITS_sf, add = T, col = 'red', pch = 19, cex = 0.3)

#combine datasets
MEDITS_prep <- data.frame(species = MEDITS$speciesName,
                          database = MEDITS$database,
                          decimalLongitude = MEDITS$hauling_longitude,
                          decimalLatitude = MEDITS$hauling_latitude)

GBIF_prep <- data.frame(species = GBIF$species,
                        database = GBIF$database,
                        decimalLongitude = GBIF$decimalLongitude,
                        decimalLatitude = GBIF$decimalLatitude)

occ <- rbind(MEDITS_prep, GBIF_prep)

#make sf object with of all occurrences
occ_sf <- st_as_sf(occ, coords = c('decimalLongitude', 'decimalLatitude'))

#extract values from raster to check which
occ_ID <- extract(ID_raster, occ_sf)

#include cellID in fish table
occ$cellID <- occ_ID$phyc_mean

#eliminate NAs (cells on land or out of the Mediterranean)
occ2 <- occ[complete.cases(occ$cellID),]

#select only one record per cell
occ3 <- unique(as.data.table(occ2), by = 'cellID')

#create spatial object with presences
occ_sf <- st_as_sf(occ3, coords = c('decimalLongitude', 'decimalLatitude'),
                   crs = crs(vars[[1]]))


#################### Study area ######################


#create a shapefile of the study area based on the variable rasters
polygons <- as.polygons(vars[[1]])
med_shp <- st_as_sf(polygons)
med_shp <- st_union(med_shp)

#make a 0.5 degree buffer around presences to avoid selection pseudo-absence
occ_buf <- st_buffer(occ_sf, dist = 50000)

#make a spatial polygon object with only one feature
occ_buf <- st_union(occ_buf)

# this may fixe possible 'duplicate vertex' errors
occ_buf <- st_make_valid(occ_buf)  #it didn't...

#transform projection to go around 'duplicate vertex'
old_crs <- crs(vars[[1]])
occ_buf <- st_transform(occ_buf, crs = 3857)
med_shp <- st_transform(med_shp, crs = 3857)

#make a holes in the study areas by the small buffer around points
pa_area <- st_difference(med_shp, occ_buf)

#transform object from "sfc_GEOMETRYCOLLECTION" to "sf"
polygons_only <- st_collection_extract(pa_area, "POLYGON")
multipolygon <- st_cast(polygons_only, "MULTIPOLYGON")
polygon <- st_union(multipolygon)
pa_area <- st_sf(geometry = polygon, attribute = 1:2)

#transform back to original projection
occ_buf <- st_transform(occ_buf, crs = old_crs)
med_shp <- st_transform(med_shp, crs = old_crs)
pa_area <- st_transform(pa_area, crs = old_crs)

#mask rasterID by pa_area
pa_raster <- mask(ID_raster, pa_area)


#################### Select pseudo-absence ######################

#extract values from raster to check which
order_ID <- extract(pa_raster, order_sf)

#include cellID in fish table
order$cellID <- order_ID$phyc_mean

#eliminate NAs (cells on land or out of the Mediterranean)
order2 <- order[complete.cases(order$cellID),]

#select only one record per cell
order3 <- unique(as.data.table(order2), by = 'cellID')

#select same number of pa as presences
pa <- order3[sample(c(1:nrow(order3)), nrow(occ3)),]

#comnine pres and pa
pa2 <- data.frame(species = 'Epinephelus aeneus',
                  database = 'GBIF',
                  decimalLongitude = pa$decimalLongitude,
                  decimalLatitude = pa$decimalLatitude,
                  cellID = pa$cellID)

#combine pr and pa
occ3$occurrence <- 1
pa2$occurrence <- 0

data <- rbind(occ3, pa2)


#save records
setwd('/Users/carloseduardoaribeiro/Documents/Collaborations/Uri Roll/Data/Epinephelus aeneus')
write.csv(data, 'Data_models_Epinephelus_aeneus.csv', row.names = F)

