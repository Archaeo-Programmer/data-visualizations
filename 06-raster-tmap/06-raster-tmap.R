library(terra)
library(tmap)
library(tidyverse)

# Download .tif image from: https://earthobservatory.nasa.gov/images/88632/mesa-verde-national-park 
MV <- rast("mesaverde_oli_2016228_geo.tif")

map1 <-
  tm_shape(MV) +
  tm_rgb(r = 1, g = 2, b = 3) +
  tm_layout(
    sepia.intensity = 0.5,
    saturation = 0.8,
    frame = TRUE,
    main.title = "Topography of Mesa Verde National Park",
    main.title.size = 1.75,
    bg.color = "#84240c",
    frame.double.line = TRUE,
    frame.lwd = 2,
    main.title.position = "center",
    main.title.color = "black"
  ) +
  tm_credits(
    text = "@Archaeo-Programmer",
    align = c("right", "BOTTOM"),
    size = 1,
    col = "white"
  ) +
  tm_compass(position = c("left", "bottom"), type = "8star")

tmap_save(map1, filename = "Mesa_Verde_sepia.png")
