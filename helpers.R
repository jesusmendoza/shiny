# Note: percent map is designed to work with the counties data set
# It may not work correctly with other data sets if their row order does 
# not exactly match the order in which the maps package plots counties
estaciones  = read.csv("data/estaciones_finales/s.csv")



m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  fitBounds(.,-99.2426,19.3502,-99.0993,19.4664)%>%
  addMarkers(.,data = estaciones,clusterOptions = markerClusterOptions())

