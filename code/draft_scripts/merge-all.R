library(sf)
library(ggplot2)
library(maptools)
library(dplyr)
library(stringr)
library(plyr)
library(zoo)



data$FIPS <- ifelse(nchar(data$FIPS) != 2 ,gsub(" ", "", paste("0",data$FIPS), fixed = TRUE), data$FIPS)

#colnames(data)[colnames(data) == "CNTLAFFI"] <- "PUBPRIVATE"

var <- read.csv("variables2.csv")
sector <- var %>% filter(varname == "SECTOR")

data <- merge(data, sector[, c("codevalue","valuelabel")], by.x = "SECTOR", by.y = "codevalue", all.x = TRUE)
data <- subset(data,select=-c(SECTOR))
colnames(data)[colnames(data) == "valuelabel"] <- "SECTOR"

pubprivate <- var %>% filter(varname == "CNTLAFFI")

data <- merge(data, pubprivate[, c("codevalue", "valuelabel")], by.x = "CNTLAFFI", by.y = "codevalue", all.x = TRUE)
data <- subset(data,select=-c(CNTLAFFI))
colnames(data)[colnames(data) == "valuelabel"] <- "PUBPRIVATE"

relaff <- var %>% filter(varname == "RELAFFIL")

data <- merge(data, relaff[, c("codevalue", "valuelabel")], by.x = "RELAFFIL", by.y = "codevalue", all.x = TRUE)
data <- subset(data,select=-c(RELAFFIL))
colnames(data)[colnames(data) == "valuelabel"] <- "RELAFFIL"

deggrant <- var %>% filter(varname == "DEGGRANT")
data <- merge(data, deggrant[, c("codevalue", "valuelabel")], by.x = "DEGGRANT", by.y = "codevalue", all.x = TRUE)
data <- subset(data,select=-c(DEGGRANT))
colnames(data)[colnames(data) == "valuelabel"] <- "DEGGRANT"

carnegie <- var %>% filter(varname == "CARNEGIE")
data <- merge(data, carnegie[, c("codevalue", "valuelabel")], by.x = "CARNEGIE", by.y = "codevalue", all.x = TRUE)
data <- subset(data,select=-c(CARNEGIE))
colnames(data)[colnames(data) == "valuelabel"] <- "CARNEGIE"





KML <- getKMLcoordinates(kmlfile = unzip(zipfile = "C:/Users/Sarah McDonald/Documents/honorsthesis/US_Dioceses.kmz", exdir = "~/honorsthesis/KML"), ignoreAltitude = TRUE)

cath_geo <- st_read("KML/doc.kml")

coords_17 <- data %>% filter(YEAR == 2017 & LINE == "Total") %>% select(UNITID, LONGITUD, LATITUDE)

coords_17$LONGITUD <- as.numeric(as.character(as.factor(coords_17$LONGITUD)))
coords_17$LATITUDE <- as.numeric(as.character(as.factor(coords_17$LATITUDE)))

coords_17 = st_as_sf(coords_17, coords = c("LONGITUD", "LATITUDE"), crs = st_crs(cath_geo))

counties <- st_read("tl_2017_us_county/tl_2017_us_county.shp")


counties = st_transform(counties, st_crs(cath_geo))

inst_county <- st_join(coords_17, counties)
inst_county <- inst_county[, c(1, 5)]
inst_county$GEOID <- as.character(as.factor(inst_county$GEOID))

count_dioceses <- read.csv("count_dioceses.csv")
count_dioceses$GEOID <- as.character(count_dioceses$GEOID)
count_dioceses$GEOID <- ifelse(nchar(count_dioceses$GEOID) != 5, gsub(" ", "", paste("0", count_dioceses$GEOID), fixed = TRUE), count_dioceses$GEOID)
inst_county<-st_set_geometry(inst_county, NULL)

inst_county_dioceses <- merge(inst_county, count_dioceses, by = "GEOID", all.x = TRUE)
inst_county_dioceses <- inst_county_dioceses[, -3]

data <- merge(data, inst_county_dioceses[, c(1:2, 6:7)], by = "UNITID", all = TRUE)

data <- subset(data, select = -c(FIPS))

socio <- read.csv("socioeconomic.csv")
socio <- socio[, -1]

socio$FIPS <- ifelse(nchar(socio$FIPS) != 5 ,gsub(" ", "", paste("0",socio$FIPS), fixed = TRUE), socio$FIPS)

data <- merge(data, socio, by.x = c("GEOID", "YEAR"), by.y = c("FIPS", "Year"), all.x = TRUE)

#write.csv(data, "data.csv")
data <- read.csv ("data.csv")


#data[levels(data$CARNEGIE)=="{Item not available}"] <- NA
data$CARNEGIE <- revalue(data$CARNEGIE, c("{Item not available}"=NA))
data$DEGGRANT <- revalue(data$DEGGRANT, c("{Not available}"=NA))



data <- data[,-c(1,2)]

School_location <- data[,c("UNITID", "INSTNM", "LONGITUD", "LATITUDE", "YEAR")]
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
Problem_Schools_in_IPEDS <- data[data$UNITID %in% Problem_IDS,]
School_locations_Final <- School_locations[,-6:-8]

# I believe you could choose to overwrite the existing INSTNM, LAT and LONG
# with your new dataset, but didn't here in case you have an issue with the creation
data <- merge(data, School_locations_Final, by = c("UNITID", "YEAR"), all.x = TRUE)


data <- subset(data,select=-c(INSTNM.x, LONGITUD.x, LATITUDE.x))

colnames(data)[colnames(data) == "INSTNM.y"] <- "INSTNM"
colnames(data)[colnames(data) == "LONGITUD.y"] <- "LONGITUD"
colnames(data)[colnames(data) == "LATITUDE.y"] <- "LATITUDE"

##############other interpolations

School_other <- data[,c("UNITID", "SECTOR", "PUBPRIVATE", "RELAFFIL", "DEGGRANT", "YEAR")]
School_others <- unique(School_other[order(School_other$UNITID, 
                                          School_other$YEAR, 
                                                 decreasing = TRUE),])


# Last obs. carried forward—closest year info will fall down (e.g.: 2009, to 2008, to 2007….)
# added na.rm = FALSE because NAs in 2017 (leading NAs) cause and error
School_others$PUBPRIVATE <- na.locf(School_others$PUBPRIVATE, na.rm = FALSE)
School_others$SECTOR <- na.locf(School_others$SECTOR)
School_others$RELAFFIL <- na.locf(School_others$RELAFFIL, na.rm = FALSE)
School_others$DEGGRANT <- na.locf(School_others$DEGGRANT, na.rm = FALSE)

School_others <- unique(School_others)

# Get the difference in Lats and longs and units to see which schools have 
# different information over the years--aka problem schools
# These may not be problems, but it will be good to keep this list if 
# we notice outliers later on, these may be it! These are schools that moved 
# from year to year and thus could have different counties.
School_others$PUBPRIVATE_Diff <- c(0,diff(round(as.numeric(School_others$PUBPRIVATE)), differences = 1))
School_others$SECTOR_Diff <- c(0,diff(round(as.numeric(School_others$SECTOR)), differences = 1))
School_others$RELAFFIL_Diff <- c(0,diff(round(as.numeric(School_others$RELAFFIL)), differences = 1))
School_others$DEGGRANT_Diff <- c(0,diff(round(as.numeric(School_others$DEGGRANT)), differences = 1))

Problem_IDS2 <- School_others[School_others$PUBPRIVATE_Diff == 0 & 
                                  (School_others$SECTOR_Diff != 0 &
                                     School_others$RELAFFIL_Diff != 0 &
                                     School_others$DEGGRANT_Diff),]$UNITID

Problem_Schools2 <- School_others[School_others$UNITID %in% Problem_IDS2,]
Problem_Schools_in_IPEDS2 <- data[data$UNITID %in% Problem_IDS2,]
School_others_Final <- School_others[,-7:-10]

# I believe you could choose to overwrite the existing INSTNM, LAT and LONG
# with your new dataset, but didn't here in case you have an issue with the creation
data <- merge(data, School_others_Final, by = c("UNITID", "YEAR"), all.x = TRUE)

data <- subset(data,select=-c(SECTOR.x, PUBPRIVATE.x, RELAFFIL.x, DEGGRANT.x))

colnames(data)[colnames(data) == "SECTOR.y"] <- "SECTOR"
colnames(data)[colnames(data) == "PUBPRIVATE.y"] <- "PUBPRIVATE"
colnames(data)[colnames(data) == "RELAFFIL.y"] <- "RELAFFIL"
colnames(data)[colnames(data) == "DEGGRANT.y"] <- "DEGGRANT"







