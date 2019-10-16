library(sf)
https://www.arcgis.com/home/item.html?id=69960b690e6e4cb3aef6fc2e5ad39456
map_dioceses <- st_read("US_Dioceses.kmz")

library(maptools)
getKMLcoordinates(textConnection(system("unzip -p US_Dioceses.kmz")))
