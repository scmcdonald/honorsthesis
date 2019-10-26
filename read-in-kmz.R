#gray.ftp.clickability.com
#https://gavinr.com/catholic-dioceses-of-the-us-map/
#https://www.arcgis.com/home/webmap/viewer.html?webmap=a945f185eab041378d595f6c0a7f628f
library(sf)
library(ggplot2)
library(maptools)
library(dplyr)
library(stringr)

KML <- getKMLcoordinates(kmlfile = unzip(zipfile = "C:/Users/Sarah McDonald/Documents/honorsthesis/US_Dioceses.kmz", exdir = "~/honorsthesis/KML"), ignoreAltitude = TRUE)

cath_geo <- st_read("KML/doc.kml")

cath_geo$Description <- as.character(as.factor(cath_geo$Description))

head(cath_geo$Description)

#get str_extract to select more

cath_geo$Description <- str_extract(cath_geo$Description, 'Diocese(.*)SUM_SUM_POP2010')

cath_geo$Description <- lapply(cath_geo$Description, function(x) gsub(c("(Diocese)?<.?t(d|r)>(SUM_SUM_POP2010)?"), "", x))

cath_geo$Description <- trimws(cath_geo$Description)
colnames(cath_geo)[colnames(cath_geo) == "Description"] <- "Dioceses"


ggplot()+
  geom_sf(data= cath_geo)

