library(sf)
library(terra)
library(png)
library(grid)
library(gridExtra)
library(ggplot2)
library(rayshader)
library(rayvista)
library(maptiles)

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

# Download elevation data and turn into a spat raster.
elev.ras <- elevatr::get_elev_raster(GP_bb, z = 13)
elev <- terra::rast(elev.ras)

# Calculate hillshade
slopes <- terra::terrain(elev, "slope", unit = "radians")
aspect <- terra::terrain(elev, "aspect", unit = "radians")
hs <- terra::shade(slopes, aspect)

# Plot hillshade as basemap.
png("GP-hillshade-contour.png", width = 2000, height = 1176)
plot(hs, col = gray(0:100 / 100), legend = FALSE, axes = FALSE,
     xlim = c(-108.80859375, -108.632771899428), ylim = c(37.2653286517636, 37.474858084971))
# Overlay with elevation.
plot(elev, col = terrain.colors(25), alpha = 0.5, legend = FALSE,
     axes = FALSE, add = TRUE)
# Add contour lines.
contour(elev, col = "grey40", add = TRUE)
dev.off()


# Overlaying orthoimage
# Use maptiles to get ESRI world imagery.
ortho <- maptiles::get_tiles(ext(-108.80859375, -108.632771899428, 37.2653286517636, 37.474858084971),
                   provider = "Esri.WorldImagery", zoom = 13)

# Plot
png("GP-hillshade-ortho.png", width = 2000, height = 1176)
plot(hs, col = gray(0:100 / 100), legend = FALSE, axes = FALSE,
     xlim = c(-108.80859375, -108.632771899428), ylim = c(37.2653286517636, 37.474858084971))
# Overlay with elevation
plot(ortho, alpha = 125, add = TRUE,
     xlim = c(-108.80859375, -108.632771899428), ylim = c(37.2653286517636, 37.474858084971))
dev.off()


# 3-D map with rayshader and rayvista
graz.3D <- rayvista::plot_3d_vista(dem = elev)
rayshader::render_snapshot(filename = "rayvista.png")


# Save all images in one png.
img1 <-  grid::rasterGrob(as.raster(png::readPNG("../10-quick-relief/rayvista.png")), interpolate = FALSE)
img2 <-  grid::rasterGrob(as.raster(png::readPNG("../10-quick-relief/GP-hillshade-ortho.png")), interpolate = FALSE)
img3 <-  grid::rasterGrob(as.raster(png::readPNG("../10-quick-relief/GP-hillshade-contour.png")), interpolate = FALSE)

#grid.arrange(img1, img3, img2, ncol = 3)

#save
g <- gridExtra::arrangeGrob(img1, img3, img2, nrow=1)
ggsave(file="all_images.png", g)