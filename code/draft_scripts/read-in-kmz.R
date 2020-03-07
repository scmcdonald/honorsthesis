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

dioceses <- cath_geo$Dioceses

#write.csv(dioceses, "dioceses_from_geo.csv")

s <- read.csv("dioceses_from_geo.csv")

#centriods for labeling later...
cent <- st_centroid(cath_geo)

counties <- st_read("tl_2017_us_county/tl_2017_us_county.shp")


counties = st_transform(counties, st_crs(cath_geo))

cent_count <- st_centroid(counties)


#Here I check which dioceses the midpoint of the county lies in. When I tried to do this for the enitre county shape, often I would get that the county was part of three different dioceses which is incorrect, so I matched the midpoint of the county to the dioceses.
count_dioceses <- st_join(cent_count, cath_geo, all.x = TRUE)

count_dioceses <- count_dioceses %>% select(STATEFP, COUNTYFP, GEOID, NAME, Name, Dioceses)
count_dioceses<-st_set_geometry(count_dioceses, NULL)

count_dioceses$STATEFP <- as.numeric(as.character(count_dioceses$STATEFP))

count_dioceses <- count_dioceses %>% filter(STATEFP <60)

write.csv(count_dioceses, "count_dioceses.csv")



write.csv(cath_geo)