#gray.ftp.clickability.com
#https://gavinr.com/catholic-dioceses-of-the-us-map/
#https://www.arcgis.com/home/webmap/viewer.html?webmap=a945f185eab041378d595f6c0a7f628f
library(sf)
library(ggplot2)
library(maptools)

KML <- getKMLcoordinates(kmlfile = unzip(zipfile = "C:/Users/Sarah McDonald/Documents/honorsthesis/US_Dioceses.kmz", exdir = "~/honorsthesis/KML"), ignoreAltitude = TRUE)

cath_geo <- st_read("KML/doc.kml")

ggplot()+
  geom_sf(data= cath_geo)



