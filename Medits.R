################################################################################
#                         Preparing Medits data                                #
################################################################################

#load packages

#list WDs
wd_medits <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Uri Roll/Data/MEDITS'

#load files
setwd(wd_medits)

TAAdata <- read.csv('TAdata-1666771796161.csv')

TBdata <- read.csv('TBdata-1666772245791.csv')  #species occurrence table but no coordinates

TCdata <- read.csv('TCdata-1666771950301.csv')   #looks like occurrence also, but the description is confusing 

taxa_medits <- read.csv('taxa_medits.csv')   #look up table for the species names


###### for now I will only deal with TBdata and try to make some sense of it

# TBdata has occurrence information. Weird code for genus and epithet that must be fixed by using a look up table. Which seems to be taxa_medits

# in TBdata, the weird species abbreviations come in two cols: genus and species
# on the other hand, the look up table decided to glue the two parts of the weird abbreviations together

# create glued column in TBdata
TBdata$weiabb <- paste0(TBdata$genus, TBdata$species)

# match glued weird abb between tables and introduce the species name in the TBdata
# I will do it in a not smart way at all. But it will work.

#create column to populate with species names
TBdata$speciesName <- NA

#forloop the hell of it!
for(i in 1:nrow(TBdata))
{
  #get the row where the species is in the look up table
  a <- which(taxa_medits$MEDITS....Code == TBdata$weiabb[i])
  
  #it seems that not all weird abbreviations are represented in the look up table
  if(length(a) != 0){
    TBdata$speciesName[i] <- taxa_medits$Scientific.Name..valid[a]
  }
  
  print(i)
}

#check how many species had NA in the match
table(is.na(TBdata$speciesName))

#TBdata does not include coordinates... where will they be? Seems I have to match TBdata to TAAdata to find out the coordinates

#found them, but they are in a weird format, I need to convert: "values resemble degrees and decimal minutes (DMM), a coordinate format often used in maritime or older geographic datasets"

#function to convert the coordinates to decimal
convert_to_dd <- function(coord) {
  degrees <- floor(coord / 100) # Extract degrees
  minutes <- coord %% 100       # Extract minutes
  decimal_degrees <- degrees + minutes / 60
  return(decimal_degrees)
}

# Apply the function creating a new col
TAAdata$decimalLongitude <- convert_to_dd(TAAdata$hauling_longitude)
TAAdata$decimalLatitude <- convert_to_dd(TAAdata$hauling_latitude)

#create a column with haul_number, day, month, year, area, country for both tables I need to relate

TBdata$new_ID <- paste(TBdata$haul_number, TBdata$day, TBdata$month, TBdata$year,
                       TBdata$area, TBdata$country, sep = "_")

TAAdata$new_ID <- paste(TAAdata$haul_number, TAAdata$day, TAAdata$month,
                        TAAdata$year, TAAdata$area, TAAdata$country, sep = '_')

#create columns to populate with decimal coordinates

##### Question #####

# Use hauling or shooting coordinates???

##### Question #####

TBdata$hauling_longitude <- NA
TBdata$hauling_latitude <- NA
TBdata$hauling_depth <- NA

#forloop the hell of it!
for(i in 1:nrow(TBdata))
{
  #get the row where the species is in the look up table
  a <- which(TAAdata$new_ID == TBdata$new_ID[i])
  
  #it seems that not all weird abbreviations are represented in the look up table
  if(length(a) == 1){
    
    TBdata$hauling_longitude[i] <- TAAdata$decimalLongitude[a]
    TBdata$hauling_latitude[i] <- TAAdata$decimalLatitude[a]
    TBdata$hauling_depth[i] <- TAAdata$hauling_depth[a]
  }
  
  print(i)
}

#save table with species and coordinates
setwd(wd_medits)
write.csv(TBdata, 'TBdata_complete.csv', row.names = F)

#select only entries of fish
TBdata_fish <- TBdata[TBdata$catfau == 'A' |
                      TBdata$catfau == 'Aa' |
                      TBdata$catfau == 'Ae' |
                      TBdata$catfau == 'Ao' ,]


#save table with fish species and coordinates
setwd(wd_medits)
write.csv(TBdata_fish, 'TBdata_fish.csv', row.names = F)




