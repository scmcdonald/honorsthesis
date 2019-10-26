#gray.ftp.clickability.com
library(sf)
library(gdalUtilities)
library(rgdal)
library(maptools)
library(ggplot2)
library(rvest)

KMZs <- list.files(path="C:/Users/Sarah McDonald/Documents/honorsthesis", pattern="*.kmz", full.names=FALSE)

LonLat <- sapply(KMZs, function(x)getKMLcoordinates(kmlfile = unzip(zipfile = paste0("C:/Users/Sarah McDonald/Documents/honorsthesis/", x),  exdir = "~/honorsthesis/KML"), ignoreAltitude = TRUE))

cath_geo <- st_read("KML/doc.kml")

ggplot()+
  geom_sf(data= cath_geo)



