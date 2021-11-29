# https://earthobservatory.nasa.gov/images/52242/four-corners-southwestern-us
library(terra)
library(tmap)
FC <- rast("fourcorners_ast_2001162_geo.tif")

map1 <-
  tm_shape(FC) +
  tm_rgb(r = 1, g = 2, b = 3) +
  # tm_layout(main.title = "Topography of Mesa Verde National Park",
  #           main.title.size = 2,
  #           main.title.color = "black") +
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

tmap_save(map1, filename = "Four_Corners.png")

