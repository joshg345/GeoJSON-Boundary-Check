library(sf)
library(ggmap)
geoJSONfile <- file.choose(new = FALSE)
.jppref.geojson <- st_read(geoJSONfile)

find_pref <- function(lon, lat, sp_polygon = .jppref.geojson) {
  res <- character(0)
  cnt = 1
  # Iterate the vector data one by one
  for(lo in lon){
    la <- lat[cnt]
    # Check whether the point belongs to any of polygons.  
    which.row <- sf::st_contains(sp_polygon, sf::st_point(c(lo, la)), sparse = FALSE) %>%  
      grep(TRUE, .)
    
    # If the point doesn't belong to any polygons, return NA.
    if (identical(which.row, integer(0)) == TRUE) {
      res <- c("no")
    } else {
      d <- sp_polygon[which.row, ]
      res <- c("yes")
    }
    cnt <- cnt+1
  }
  return (res)
}

# Select the file from the file chooser
fileToLoad <- file.choose(new = FALSE)

# Read in the CSV data and store it in a variable 
origAddress <- read.csv(fileToLoad, stringsAsFactors = FALSE)

register_google(key = "APIKEYHERE")

outputFile <- file.choose(new = TRUE)


for(i in 1:nrow(origAddress)){
  result <- geocode(origAddress$Address[i], output = "latlona", source = "google")
  inTerritory <- find_pref(as.numeric(result[1]),as.numeric(result[2]))
  if (inTerritory == "yes") {
    output <- data.frame("First Name" = origAddress$First.Name[i], "Last Name" = origAddress$Last.Name[i],"Phone Number" = origAddress$Phone.Number[i], "Address" = origAddress$Address[i], "Latitude" = as.numeric(result[2]), "Longitude" = as.numeric(result[1]))
    write.table(output, outputFile, append = i > 1, sep= ",", row.names = FALSE, col.names = i == 1)
  }
}
