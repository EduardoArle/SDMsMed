################################################################################
#                      Counting occurrences per sps                            #
################################################################################

#load packages
library(plyr); library(sf)

#list WDs
wd_medits <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Uri Roll/Data/MEDITS'
wd_med <- '/Users/carloseduardoaribeiro/Documents/Post-doc/CNA marine/Data/Intersect_EEZ_IHO_v4_simplified'

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

#visualise
hist(rec_sps$V1)
hist(sps_50$V1)

#create spatial object from fish occurrences
fish_sf <- st_as_sf(fish_table,
                    coords = c('hauling_longitude', 'hauling_latitude'))

#load Mediterranean SHP
med <- st_read(layer = 'Mediterranean_regions', dsn = wd_med)

#visualise
plot(st_geometry(med), border = NA, col = 'azure3', bg = 'cornsilk1')
plot(fish_sf, add = T, pch = 19, col = 'darkgreen', cex = 0.3)
text(15, 47, 'All fish', cex = 2, font = 2)
