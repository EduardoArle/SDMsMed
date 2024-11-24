################################################################################
#                      Preparing BioOracle variables                           #
################################################################################


#load packages
library(sf); library(terra)

#list WDs
wd_shp <- '/Users/carloseduardoaribeiro/Documents/Post-doc/CNA marine/Data/Intersect_EEZ_IHO_v4_simplified'
wd_raw_vars_pr <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Uri Roll/Data/BioOracle/Raw_Present'
wd_vars_pr <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Uri Roll/Data/BioOracle/Present'
wd_raw_vars_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Uri Roll/Data/BioOracle/Raw_2090_ssp585'
wd_vars_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Uri Roll/Data/BioOracle/2090'

#load Mediterranean SHP
med <- st_read(layer = 'Mediterranean_regions', dsn = wd_med)


###################
##### PRESENT #####
###################

#load variables
setwd(wd_raw_vars_pr)

vars <- lapply(list.files(), rast)
names(vars) <- lapply(vars, names)

#mask, crop and save variables by study area
setwd(wd_vars_pr)
crop_vars_pr <- list() #to check
for(i in 1:length(vars))
{
  mask_vars <- mask(vars[[i]], med)
  crop_vars_pr[[i]] <- crop(mask_vars, med)
  writeRaster(crop_vars_pr[[i]],
              paste0(names(crop_vars_pr[[i]]),'.tif'))
  print(i)
}



####################
####### 2090 #######
####################

#load variables
setwd(wd_raw_vars_2090)

vars_2090 <- lapply(list.files(), rast)
names(vars_2090) <- lapply(vars_2090, names)

#mask, crop and save variables by study area
setwd(wd_vars_2090)
crop_vars_2090 <- list() #to check
for(i in 1:length(vars_2090))
{
  mask_vars_2090 <- mask(vars_2090[[i]], med)
  crop_vars_2090[[i]] <- crop(mask_vars_2090, med)
  writeRaster(crop_vars_2090[[i]],
              paste0(names(crop_vars_2090[[i]]),'.tif'))
  print(i)
}




