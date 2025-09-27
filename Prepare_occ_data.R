#load packages
library(sf); library(terra); library(data.table)

#list WDs
wd_data <- '/Users/carloseduardoaribeiro/Documents/Post-doc/ENMs_med/Data'
wd_occ <- '/Users/carloseduardoaribeiro/Documents/Post-doc/ENMs_med/Processes_data'
wd_vars <- '/Users/carloseduardoaribeiro/Documents/Post-doc/ENMs_med/Variables_present'
wd_pr_PA <- '/Users/carloseduardoaribeiro/Documents/Post-doc/ENMs_med/pr_PA'
wd_plots <- '/Users/carloseduardoaribeiro/Documents/Post-doc/ENMs_med/Plots_pr_PA'

#get species list
setwd(wd_occ)
sps_list <- list.files()

#load all points to select pseudo absences
setwd(wd_data)
load('Clean_Medata_lis.Rdata')

#laod one variable for CRS and ploting
setwd(wd_vars)
var <- rast(list.files()[2])

#determine CRS we are using
CRS <- "GEOGCRS[\"WGS 84 (CRS84)\",\n    DATUM[\"World Geodetic System 1984\",\n        ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n            LENGTHUNIT[\"metre\",1]]],\n    PRIMEM[\"Greenwich\",0,\n        ANGLEUNIT[\"degree\",0.0174532925199433]],\n    CS[ellipsoidal,2],\n        AXIS[\"geodetic longitude (Lon)\",east,\n            ORDER[1],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        AXIS[\"geodetic latitude (Lat)\",north,\n            ORDER[2],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n    USAGE[\n        SCOPE[\"unknown\"],\n        AREA[\"World\"],\n        BBOX[-90,-180,90,180]],\n    ID[\"OGC\",\"CRS84\"]]"

#make ID raster
ID_rast <- var
non_na_cells <- which(!is.na(values(ID_rast)))
new_vals <- rep(NA, ncell(ID_rast))
new_vals[non_na_cells] <- seq_along(non_na_cells)
ID_rast[] <- new_vals
names(ID_rast) <- 'ID'
crs(ID_rast) <- CRS

#make a polygon with mediterranean boundaries
coords_med <- matrix(c(-6, 30, 38, 30, 38, 46, 2, 46, -6, 37, -6, 30),  
                     ncol = 2, byrow = TRUE)

poly_med <- st_polygon(list(coords_med))
study_area <- st_sf(geometry = st_sfc(poly_med, crs = CRS))

#crop ID raster to mediterranean resolution
cropped <- crop(ID_rast, study_area)   
ID_med <- mask(cropped, study_area) 

#make an sf object of all presences (for PA selection)
all_pr <- medata[medata$sp.n > 0,]
all_pr_sf <- st_as_sf(all_pr, coords = c('lon', 'lat'), crs = CRS)

#eliminate points on land
ID <- extract(ID_rast, all_pr_sf)
all_pr$ID <- ID[,2]
all_pr <- all_pr[complete.cases(all_pr$ID),]

#keep only one record per cell
all_pr <- as.data.frame(unique(as.data.table(all_pr), by = 'ID'))
all_pr_sf <- st_as_sf(all_pr, coords = c('lon', 'lat'), crs = CRS)

#keep only on point per cell

#loop through species to select pseudo absences
for(i in 5:length(sps_list))
{
  setwd(wd_occ)
  sps <- read.csv(sps_list[[i]])
  
  #create spat object occ
  sps_sf <- st_as_sf(sps, coords = c('lon', 'lat'),  crs = CRS)
  
  #make a 10 km buffer around presences to avoid selection pseudo-absence
  sps_sf_buf <- st_buffer(sps_sf, dist = 10000)
  
  #make a spatial polygon object with only one feature
  no_pa <- st_union(sps_sf_buf)
  
  # this fixes possible 'duplicate vertex' errors
  no_pa <- st_make_valid(no_pa) 
  
  #make a holes in the study areas by the small buffer around points
  pa_area <- st_difference(study_area, no_pa)
  pa_area <- st_make_valid(pa_area)
  
  #define number of pseudo abs to be created (same as presences)
  n_pa <- nrow(sps)
  
  #select candidates for pa (other points same taxon) within in the pa_area
  pa_1 <- vapply(st_intersects(all_pr_sf, pa_area), 
                    function(x) if (length(x)==0) NA_integer_ else x[1],
                    FUN.VALUE = 1)
  
  pa_2 <- all_pr[!is.na(pa_1),]
  
  #check whether there are enough points for PA
  if(nrow(pa_2) >= n_pa){
    #randomly select n pa points amongst the occ of other urchins
    pa_3 <- pa_2[sample(c(1:nrow(pa_2)), n_pa),]
  }else{
    #randomly select n pa points amongst the occ of other urchins
    pa_3 <- pa_2
    warning(paste0(n_pa, ' presences, ', nrow(pa_2), ' absences.'))
  }

  #create column with occurrence status info
  pa_3$occurrence <- 0
  
  #eliminate column with ID from sps
  #sps <- sps[,-ncol(sps)]
  
  #join pr and PA
  sps_all <- rbind(sps, pa_3)
  
  #make spatial object
  sps_all_sf <- st_as_sf(sps_all, coords = c('lon', 'lat'), crs = CRS)
  
  #save images of the maps
  setwd(wd_plots)
  pdf(file=paste0(unique(sps$species),".pdf"))
  
  #plot background med
  plot(ID_med, col = 'grey80',
       main = gsub('csv', '', (gsub('\\.', ' ', sps_list[[i]]))),
       box = NA, legend = NA, axes = NA)
  
  #plot occ
  plot(sps_all_sf[which(sps_all_sf$occurrence == 1),], add = T,
       pch = 19, col = 'darkgreen')
  plot(sps_all_sf[which(sps_all_sf$occurrence == 0),], add = T,
       pch = 19, col = 'orange')
  
  dev.off()
  
  #save table
  setwd(wd_pr_PA)
  write.csv(sps_all,
            paste0(gsub('csv', '', (gsub('\\.', ' ', sps_list[[i]]))), '.csv'),
            row.names = F)
  
  print(i)
}












#prepare tables to save
coords_dia <- as.data.frame(st_coordinates(occ_dia_sf3)) #get coordinates
coords_PA <- as.data.frame(st_coordinates(pa_dia3)) #get coordinates

names(coords_dia) <- c('decimalLongitude', 'decimalLatitude')
names(coords_PA) <- c('decimalLongitude', 'decimalLatitude')

occ_dia_sf4 <- cbind(as.data.frame(occ_dia_sf3), coords_dia)
pa_dia4 <- cbind(as.data.frame(pa_dia3), coords_PA)

#create col with occurrence status
occ_dia_sf4$occurrence <- 1
pa_dia4$occurrence <- 0

#join pr and PA
dia_pr_PA <- rbind(occ_dia_sf4, pa_dia4)

#save presence and pseudo-absences
setwd(wd_occ)

write.csv(dia_pr_PA, 'Diadema_setosum_pr_PA_equinodermata_10km.csv',row.names = F)


# Mediterranean

#load data for mediterranean
setwd(wd_occ)
occ_pa <- read.csv('Echinodermata_med.csv', sep = '\t')

#eliminate  Diadema setosum from sea urchin data
occ_others <- occ_pa[occ_pa$species != 'Diadema setosum',]

#load data for Diadema (currated and validade by Iris and Anton)
setwd(wd_occ)
occ_med <- read.csv('Diadema_setosum_SDM_MED.csv')

#eliminate occurrences in the west mediterranean
west <- which(occ_med$countryCode == 'FR' |
              occ_med$countryCode == 'ES')
occ_med2 <- occ_med[-west,]

#load one variable to thin the data
setwd(wd_variables)
var <- rast(list.files()[1])

#make a polygon of the study area with same coords to download GBIF data
coords_med <- matrix(c(-6, 30, 38, 30, 38, 46, 2, 46, -6, 37, -6, 30),  
                     ncol = 2, byrow = TRUE)

poly_med <- st_polygon(list(coords_med))
study_area_med <- st_sf(geometry = st_sfc(poly_med, crs = crs(var)))

plot(study_area_med, add = T)


#create spat object occ
occ_med_sf <- st_as_sf(occ_med2,
                       coords = c('decimalLongitude', 'decimalLatitude'),
                       crs = crs(var))

occ_others_med_sf <- st_as_sf(occ_others,
                          coords = c('decimalLongitude', 'decimalLatitude'),
                          crs = crs(var))

#extract cell ID 
ID <- extract(ID_rast, occ_med_sf)
occ_med_sf$ID <- ID[,2]

ID <- extract(ID_rast, occ_others_med_sf)
occ_others_med_sf$ID <- ID[,2]

#keep only cells in the ocean
occ_med_sf2 <- occ_med_sf[!is.na(occ_med_sf$ID),]
occ_others_med_sf2 <- occ_others_med_sf[!is.na(occ_others_med_sf$ID),]

#eliminate duplicated cells in Diadema and PA
occ_med_sf3 <- unique(as.data.table(occ_med_sf2), by = 'ID')
occ_others_med_sf3 <- unique(as.data.table(occ_others_med_sf2),
                         by = c('species', 'ID'))

#retransform objects into sf
occ_med_sf3 <- st_as_sf(occ_med_sf3)
occ_others_med_sf3 <- st_as_sf(occ_others_med_sf3)

#make a 50 km buffer around presences to avoid selection pseudo-absence
med_sf_buf <- st_buffer(occ_med_sf3, dist = 10000)

#make a spatial polygon object with only one feature
no_pa_med <- st_union(med_sf_buf)

# this fixes possible 'duplicate vertex' errors
no_pa_med <- st_make_valid(no_pa_med) 

#make a holes in the study areas by the small buffer around points
pa_area_med <- st_difference(study_area_med, no_pa_med)
pa_area_med <- st_make_valid(pa_area_med)

#define number of pseudo abs to be created (same as presences)
n_pa_med <- nrow(occ_med_sf3)

#select candidates for pa (other points same taxon) within in the pa_area
pa_med1 <- vapply(st_intersects(occ_others_med_sf3, pa_area_med), 
                  function(x) if (length(x)==0) NA_integer_ else x[1],
                  FUN.VALUE = 1)

pa_med2 <- occ_others_med_sf3[!is.na(pa_med1),]

#randomly select n pa points amongst the occ of other urchins
pa_med3 <- pa_med2[sample(c(1:nrow(pa_med2)), n_pa_med),]

#visualise PA are
plot(st_geometry(pa_area_med), col = '#d5d5d5')
plot(occ_med_sf3, add = T, col = '#000000', bg = '#30A530', pch = 21, cex = 0.4)

#visualise pres and PA
plot(st_geometry(study_area_med), border = NA, col = '#d5d5d5')
plot(ID_rast, add = T)
plot(occ_med_sf3, add = T, col = '#30A530', pch = 19, cex = 0.8)
plot(pa_med3, add = T, col = '#d58920', pch = 19, cex = 0.8)

#prepare tables to save
coords_med <- as.data.frame(st_coordinates(occ_med_sf3)) #get coordinates
coords_PA_med <- as.data.frame(st_coordinates(pa_med3)) #get coordinates

names(coords_med) <- c('decimalLongitude', 'decimalLatitude')
names(coords_PA_med) <- c('decimalLongitude', 'decimalLatitude')
occ_med_sf4 <- cbind(as.data.frame(occ_med_sf3), coords_med)
pa_med4 <- cbind(as.data.frame(pa_med3), coords_PA_med)

#create col with occurrence status
occ_med_sf4$occurrence <- 1
pa_med4$occurrence <- 0

#join pr and PA (tables have different ncol)

# Find common columns
common_cols <- intersect(names(occ_med_sf4), names(pa_med4))

# Restrict both to common columns
occ_med_sf4_sub <- occ_med_sf4[ , common_cols, drop = FALSE]
pa_med4_sub <- pa_med4[ , common_cols, drop = FALSE]

med_pr_PA <- rbind(occ_med_sf4_sub, pa_med4_sub)

#save presence and pseudo-absences
setwd(wd_occ)

write.csv(med_pr_PA, 'Diadema_setosum_pr_PA_med_10km.csv')

