################################################################################
#                      Counting occurrences per sps                            #
################################################################################

#load packages
library(plyr); library(sf); library(terra)

#list WDs
wd_medits <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Uri Roll/Data/MEDITS'
wd_med <- '/Users/carloseduardoaribeiro/Documents/Post-doc/CNA marine/Data/Intersect_EEZ_IHO_v4_simplified'
wd_variables <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Uri Roll/Data/BioOracle/Present'

#load fish table
setwd(wd_medits)
fish_table <- read.csv('TBdata_fish.csv')

#eliminate occurrences without coordinates or without species name
fish_table <- fish_table[complete.cases(fish_table$hauling_longitude),]
fish_table <- fish_table[complete.cases(fish_table$hauling_latitude),]
fish_table <- fish_table[complete.cases(fish_table$speciesName),]

#count the number of species
sps_n <- length(unique(fish_table$speciesName))
sps_n

#count total records
occ_n <- nrow(fish_table)
occ_n

#count records with depth
occ_depth <- length(!is.na(fish_table$hauling_depth))
occ_depth

#count records per species
rec_sps <- ddply(fish_table, .(speciesName), nrow)

#count species with at least 50 records
sps_50 <- rec_sps[rec_sps$V1 >= 50,]
nrow(sps_50)

#load one variable layer
setwd(wd_variables)
var <- rast(list.files()[[1]])

#visualise
hist(rec_sps$V1)
hist(sps_50$V1)

#create spatial object from fish occurrences
fish_sf <- st_as_sf(fish_table,
                    coords = c('hauling_longitude', 'hauling_latitude'))

#load Mediterranean SHP
med <- st_read(layer = 'Mediterranean_regions', dsn = wd_med)

#create an ID raster from one of the variables
ID_raster <- var
ID_raster[which(!is.na(var[]))] <- c(1:length(which(!is.na(var[]))))

#extract values from raster to check which
fish_sf_ID <- extract(ID_raster, fish_sf)

#include cellID in fish table
fish_table$cellID <- fish_sf_ID$phyc_mean

#visualise
plot(var, col = 'azure3', colNA = 'cornsilk1',
     box = NA, legend = NA, axes = NA)
plot(fish_sf, add = T, pch = 19, col = 'darkgreen', cex = 0.3)
text(15, 47, 'All fish', cex = 2, font = 2)

#save fish table with cell ID
