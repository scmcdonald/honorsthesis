library("zoo")

IPEDS <- read.csv("data.csv")
IPEDS <- IPEDS[,-c(1,2)]

School_location <- IPEDS[,c("UNITID", "INSTNM", "LONGITUD", "LATITUDE", "YEAR")]
School_locations <- unique(School_location[order(School_location$UNITID, 
  School_location$YEAR, 
  decreasing = TRUE),])

# Your latitude and longitude variables are factors--NO! They should be numeric
School_locations$LONGITUD <- as.numeric(paste(School_locations$LONGITUD))
School_locations$LATITUDE <- as.numeric(paste(School_locations$LATITUDE))

# Last obs. carried forward—closest year info will fall down (e.g.: 2009, to 2008, to 2007….)
School_locations$UNITID <- na.locf(School_locations$UNITID)
School_locations$INSTNM <- na.locf(School_locations$INSTNM)
School_locations$LONGITUD <- na.locf(School_locations$LONGITUD)
School_locations$LATITUDE <- na.locf(School_locations$LATITUDE)

School_locations <- unique(School_locations)

# Get the difference in Lats and longs and units to see which schools have 
# different information over the years--aka problem schools
# These may not be problems, but it will be good to keep this list if 
# we notice outliers later on, these may be it! These are schools that moved 
# from year to year and thus could have different counties.
School_locations$ID_Diff <- c(0,diff(round(as.numeric(School_locations$UNITID)), differences = 1))
School_locations$Lat_Diff <- c(0,diff(round(as.numeric(School_locations$LATITUDE)), differences = 1))
School_locations$Long_Diff <- c(0,diff(round(as.numeric(School_locations$LONGITUD)), differences = 1))
Problem_IDS <- School_locations[School_locations$ID_Diff == 0 & 
                                  (School_locations$Long_Diff != 0 &
                                     School_locations$Lat_Diff != 0),]$UNITID
Problem_Schools <- School_locations[School_locations$UNITID %in% Problem_IDS,]
Problem_Schools_in_IPEDS <- IPEDS[IPEDS$UNITID %in% Problem_IDS,]
School_locations_Final <- School_locations[,-6:-8]

# I believe you could choose to overwrite the existing INSTNM, LAT and LONG
# with your new dataset, but didn't here in case you have an issue with the creation
IPEDS_2 <- merge(IPEDS, School_locations_Final, by = c("UNITID", "YEAR"))






