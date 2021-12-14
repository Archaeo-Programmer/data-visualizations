library(sf)
library(terra)

# Goodman Point Watershed bounding box.
GP_bb <-
  c(
    "xmin" = -108.77838,
    "xmax" = -108.69598,
    "ymin" = 37.32895,
    "ymax" = 37.43970
  ) %>%
  sf::st_bbox() %>%
  sf::st_as_sfc() %>%
  sf::st_as_sf(crs = 4326) %>%
  sf::st_transform(crs = 4326)

# Download elevation data
elev.ras <- elevatr::get_elev_raster(GP_bb, z = 13)
elev <- terra::rast(elev.ras)

# Calculate hillshade
slopes <- terra::terrain(elev, "slope", unit = "radians")
aspect <- terra::terrain(elev, "aspect", unit = "radians")
hs <- terra::shade(slopes, aspect)

# Plot hillshading as basemap
plot(hs, col = gray(0:100 / 100), legend = FALSE, axes = FALSE,
     xlim = c(-108.80859375, -108.632771899428), ylim = c(37.2653286517636, 37.474858084971))
# Overlay with elevation
plot(elev, col = terrain.colors(25), alpha = 0.5, legend = FALSE,
     axes = FALSE, add = TRUE)
# Add contour lines
contour(elev, col = "grey40", add = TRUE)


# Overlaying orthoimage

library(maptiles)
ortho <- get_tiles(ext(-108.80859375, -108.632771899428, 37.2653286517636, 37.474858084971),
                   provider = "Esri.WorldImagery", zoom = 13)

# Plot
plot(hs, col = gray(0:100 / 100), legend = FALSE, axes = FALSE,
     xlim = c(-108.80859375, -108.632771899428), ylim = c(37.2653286517636, 37.474858084971))
# Overlay with elevation
plot(ortho, alpha = 125, add = TRUE,
     xlim = c(-108.80859375, -108.632771899428), ylim = c(37.2653286517636, 37.474858084971))


# 3-D map with rayshader and rayvista

library(rayshader)
library(rayvista)

graz.3D <- plot_3d_vista(dem = elev)
render_snapshot(filename = "rayvista.png")