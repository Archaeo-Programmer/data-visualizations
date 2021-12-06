library(rayshader)
library(raster)
library(osmdata)
library(sf)
library(tidyverse)

# Read in the DEM for southwestern Colorado, including Mesa Verde.
mesaverde <- raster::raster("./03-raster-timelapse/EXTRACTIONS/ned_SWCol/NED/ned_SWCol_NED_1.tif")

# Extent of Mesa Verde National Park.
lat_range <- c(37.156469, 37.350311)
long_range <- c(-108.6, -108.3)

convert_coords <- function(lat, long, from = CRS("+init=epsg:4326"), to) {
  data = data.frame(long=long, lat=lat)
  coordinates(data) <- ~ long+lat
  proj4string(data) = from
  #Convert to coordinate system specified by EPSG code
  xy = data.frame(sp::spTransform(data, to))
  colnames(xy) = c("x","y")
  return(unlist(xy))
}

# Crop raster to just the extent of Mesa Verde National Park.
bbox <- convert_coords(lat = lat_range, long = long_range, to = crs(mesaverde))
extent_zoomed <- raster::extent(bbox[1], bbox[2], bbox[3], bbox[4])
mesaverde_zoom <- raster::crop(mesaverde, extent_zoomed)
mesaverde_zoom_mat <- rayshader::raster_to_matrix(mesaverde_zoom)


# Create a base map.
base_map <- mesaverde_zoom_mat %>%
  height_shade(texture = rev((grDevices::colorRampPalette(
    c("#6AA85B", "#D9CC9A", "#FFFFFF")
  ))(256))) %>%
  add_overlay(
    sphere_shade(
      mesaverde_zoom_mat,
      texture = "desert",
      zscale = 3,
      colorintensity = 4
    ),
    alphalayer = 0.4
  ) %>%
  add_shadow(lamb_shade(mesaverde_zoom_mat, zscale = 8), 0.6) %>%
  add_shadow(ambient_shade(mesaverde_zoom_mat, zscale = 8), 0.4) %>%
  add_shadow(texture_shade(
    mesaverde_zoom_mat,
    detail = 8 / 10,
    contrast = 9,
    brightness = 13
  ),
  0.3)

# OpenStreetMap requires the bounding box to be in a particular format. 
osm_bbox <- c(long_range[1], lat_range[1], long_range[2], lat_range[2])

# Extract particular features that we will add to the map separately.

# Get highway data for around Mesa Verde.
mesaverde_highway <- osmdata::opq(osm_bbox) %>% 
  osmdata::add_osm_feature("highway") %>% 
  osmdata::osmdata_sf()

# Convert highway data into an sf object.
mesaverde_lines <- sf::st_transform(mesaverde_highway$osm_lines, crs = crs(mesaverde))

# Separate the highway data to pull out different trails and pathways.
mesaverde_trails <- mesaverde_lines %>% 
  dplyr::filter(highway %in% c("path","bridleway"))

mesaverde_footpaths <- mesaverde_lines %>% 
  dplyr::filter(highway %in% c("footway"))

mesaverde_roads <- mesaverde_lines %>% 
  dplyr::filter(!highway %in% c("path","bridleway", "footway"))

# Get water data for around Mesa Verde.
mesaverde_water_lines <- osmdata::opq(osm_bbox) %>% 
  osmdata::add_osm_feature("waterway") %>% 
  osmdata::osmdata_sf()

# Convert water data into sf object.
mesaverde_streams <- sf::st_transform(mesaverde_water_lines$osm_lines, crs = crs(mesaverde))

# Filter to only the major rivers.
mesaverde_rivers <- mesaverde_streams %>% 
  dplyr::filter(name %in% c("McElmo Creek", "Mancos River", "Mud Creek", "West Fork Mud Creek", "East Fork Mud Creek"))

# Create a stream layer to add to the map
stream_layer <- rayshader::generate_line_overlay(mesaverde_rivers,extent = extent_zoomed,
                                     linewidth = 4, color="skyblue2", 
                                     heightmap = mesaverde_zoom_mat)

# Get tourism data for around Mesa Verde.
mesaverde_tourism <- osmdata::opq(osm_bbox) %>% 
  osmdata::add_osm_feature("tourism") %>% 
  osmdata::osmdata_sf()

# Convert tourism data into sf object.
mesaverde_tourism_points <- sf::st_transform(mesaverde_tourism$osm_points, crs = crs(mesaverde))

# Filter to specific sites.
mesaverde_attractions <- mesaverde_tourism_points %>% 
  dplyr::filter(tourism == "attraction") %>% 
  dplyr::filter(name %in% c("Cliff Palace", "Balcony House", "Spruce Tree House", "The Knife Edge"))


# Final Map
watercolor <- "#2a89b3"
maxcolor <- "#e6dbc8"
mincolor <- "#b6bba5"
contour_color <- "#7d4911"


mesaverde_zoom_mat %>% 
  height_shade(texture = rev((grDevices::colorRampPalette(
    c("#6AA85B", "#D9CC9A", "#FFFFFF")
  ))(256))) %>% 
  add_shadow(lamb_shade(mesaverde_zoom_mat),0.2) %>% 
  add_overlay(generate_contour_overlay(mesaverde_zoom_mat, color = contour_color, 
                                       linewidth = 2, levels=seq(min(mesaverde_zoom_mat), max(mesaverde_zoom_mat), by=5)),alphalayer = 0.5) %>% 
  add_overlay(height_shade(mesaverde_zoom_mat,texture = "white"), alphalayer=0.35) %>% 
  add_overlay(generate_line_overlay(mesaverde_rivers,extent = extent_zoomed,
                                    linewidth = 3, color=watercolor, 
                                    heightmap = mesaverde_zoom_mat)) %>% 
  add_overlay(generate_line_overlay(mesaverde_footpaths,extent = extent_zoomed,
                                    linewidth = 3, color="black", 
                                    heightmap = mesaverde_zoom_mat)) %>% 
  add_overlay(generate_line_overlay(mesaverde_trails,extent = extent_zoomed,
                                    linewidth = 3, color="black", lty=2,
                                    heightmap = mesaverde_zoom_mat)) %>% 
  add_overlay(generate_line_overlay(mesaverde_roads,extent = extent_zoomed,
                                    linewidth = 6, color="black",
                                    heightmap = mesaverde_zoom_mat)) %>% 
  add_overlay(generate_line_overlay(mesaverde_roads,extent = extent_zoomed,
                                    linewidth = 5, color="white",
                                    heightmap = mesaverde_zoom_mat)) %>% 
  add_overlay(generate_label_overlay(mesaverde_attractions, extent = extent_zoomed,
                                     text_size = 2, point_size = 2, color = "black", 
                                     halo_color = "#e6e1db", halo_expand = 5, 
                                     halo_blur = 2, halo_alpha = 0.8,
                                     seed=1, heightmap = mesaverde_zoom_mat, 
                                     data_label_column = "name")) %>% 
  add_overlay(generate_label_overlay("Mancos River", x = -108.35877, y = 37.25772, extent = extent_zoomed,
                                     text_size = 2, point_size = 2, color = "black", 
                                     halo_color = "#e6e1db", halo_expand = 5, 
                                     halo_blur = 2, halo_alpha = 0.8,
                                     seed=1, heightmap = mesaverde_zoom_mat, 
                                     data_label_column = "name")) %>% 
  add_overlay(generate_scalebar_overlay(extent = extent_zoomed, length = 6000,
                                        text_color = "black",
                                        heightmap = montereybay,
                                        latlong=TRUE, text_size = 1,
                                        halo_color = "#e6e1db", halo_expand = 2,
                                        halo_blur = 1, halo_alpha = 0.8)) %>% 
  plot_map(title_text="Mesa Verde National Park, Colorado", title_color = "white",
           title_bar_alpha = 1, title_bar_color = "black")
