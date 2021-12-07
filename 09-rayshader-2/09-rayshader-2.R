library(rayshader)

# Mesa Verde bounding box.
bb <-
  c(
    "xmin" = -108.6,
    "xmax" = -108.3,
    "ymin" = 37.156469,
    "ymax" = 37.350311
  ) %>%
  sf::st_bbox() %>%
  sf::st_as_sfc() %>%
  sf::st_as_sf(crs = 4326) %>%
  sf::st_transform(crs = 4326)

mv <- raster::raster("../03-raster-timelapse/EXTRACTIONS/ned_SWCol/NED/ned_SWCol_NED_1.tif")

mv_zoom <- raster::crop(mv, bb)

elmat <- rayshader::raster_to_matrix(mv_zoom)

base_map <- elmat %>%
  height_shade(texture = rev((grDevices::colorRampPalette(
    c("#575930", "#8d9779", "#a0906b", "#e8e0c9")
  ))(256))) %>%
  add_overlay(
    sphere_shade(
      elmat,
      texture = "desert",
      zscale = 3,
      colorintensity = 4
    ),
    alphalayer = 0.55
  ) %>%
  add_shadow(lamb_shade(elmat, zscale = 8), 0.6) %>%
  add_shadow(ambient_shade(elmat, zscale = 1/5), 0.6) %>%
  add_shadow(texture_shade(
    elmat,
    detail = 8 / 10,
    contrast = 9,
    brightness = 12
  ),
  0.15)
# plot_map(base_map)

# OpenStreetMap requires the bounding box to be in a particular format. 
osm_bbox <- c(-108.6, 37.156469, -108.3, 37.350311)

# Extract particular features that we will add to the map separately.

# Get highway data for around Mesa Verde.
mesaverde_highway <- osmdata::opq(osm_bbox) %>% 
  osmdata::add_osm_feature("highway") %>% 
  osmdata::osmdata_sf()

# Convert highway data into an sf object.
mesaverde_lines <- sf::st_transform(mesaverde_highway$osm_lines, crs = raster::crs(mv_zoom))

# Separate the highway data to pull out different trails and pathways.
mesaverde_trails <- mesaverde_lines %>% 
  dplyr::filter(highway %in% c("path","bridleway"))

mesaverde_footpaths <- mesaverde_lines %>% 
  dplyr::filter(highway %in% c("footway"))

mesaverde_roads <- mesaverde_lines %>% 
  dplyr::filter(!highway %in% c("path","bridleway", "footway"))

trails_layer <- generate_line_overlay(mesaverde_footpaths, extent = raster::extent(mv_zoom),
                                      linewidth = 4, color="black", 
                                      heightmap = elmat) %>% 
  add_overlay(generate_line_overlay(mesaverde_footpaths,extent = raster::extent(mv_zoom),
                                    linewidth = 3, color="white",
                                    heightmap = elmat)) %>%
  add_overlay(generate_line_overlay(mesaverde_trails,extent = raster::extent(mv_zoom),
                                    linewidth = 3, color="black", lty=3, offset = c(2,-2),
                                    heightmap = elmat)) %>%
  add_overlay(generate_line_overlay(mesaverde_trails,extent = raster::extent(mv_zoom),
                                    linewidth = 3, color="white", lty=3,
                                    heightmap = elmat)) %>%
  add_overlay(generate_line_overlay(mesaverde_roads,extent = raster::extent(mv_zoom),
                                    linewidth = 5, color="white",
                                    heightmap = elmat))

trails_layer2 <- generate_line_overlay(mesaverde_footpaths, extent = raster::extent(mv_zoom),
                                       linewidth = 3, color="black", 
                                       heightmap = elmat) %>% 
  add_overlay(generate_line_overlay(mesaverde_footpaths,extent = raster::extent(mv_zoom),
                                    linewidth = 2, color="white",
                                    heightmap = elmat)) %>%
  add_overlay(generate_line_overlay(mesaverde_trails,extent = raster::extent(mv_zoom),
                                    linewidth = 3, color="black", lty=3, offset = c(2,-2),
                                    heightmap = elmat)) %>%
  add_overlay(generate_line_overlay(mesaverde_trails,extent = raster::extent(mv_zoom),
                                    linewidth = 3, color="white", lty=3,
                                    heightmap = elmat))

# Get water data for around Mesa Verde.
mesaverde_water_lines <- osmdata::opq(osm_bbox) %>% 
  osmdata::add_osm_feature("waterway") %>% 
  osmdata::osmdata_sf()

# Convert water data into sf object.
mesaverde_streams <- sf::st_transform(mesaverde_water_lines$osm_lines, crs = raster::crs(mv_zoom))

# Filter to only the major rivers.
mesaverde_rivers <- mesaverde_streams %>% 
  dplyr::filter(name %in% c("McElmo Creek", "Mancos River", "Mud Creek", "West Fork Mud Creek", "East Fork Mud Creek"))

# Create a stream layer to add to the map
stream_layer <- rayshader::generate_line_overlay(mesaverde_rivers, extent = raster::extent(mv_zoom),
                                                 linewidth = 4, color="#8CA9BE", 
                                                 heightmap = elmat)

# Get tourism data for around Mesa Verde.
mesaverde_tourism <- osmdata::opq(osm_bbox) %>% 
  osmdata::add_osm_feature("tourism") %>% 
  osmdata::osmdata_sf()

# Convert tourism data into sf object.
mesaverde_tourism_points <- sf::st_transform(mesaverde_tourism$osm_points, crs = raster::crs(mv_zoom))

# Filter to specific sites.
mesaverde_attractions <- mesaverde_tourism_points %>% 
  dplyr::filter(tourism == "attraction") %>% 
  dplyr::filter(name %in% c("Cliff Palace", "Balcony House", "Spruce Tree House", "The Knife Edge"))



base_map %>% 
  add_overlay(stream_layer, alphalayer = 0.8) %>% 
  # add_overlay(trails_layer2) %>%
  add_overlay(generate_point_overlay(mesaverde_attractions, extent = raster::extent(mv_zoom),
                                     size = 2, color = "black", heightmap = elmat)) %>%
  # add_overlay(generate_point_overlay(mesaverde_attractions, extent = raster::extent(mv_zoom),
  #                                    size = 3, color = "white", heightmap = elmat)) %>%
  plot_3d(elmat, zscale = 6, theta = 0, phi=45, zoom = .56, windowsize = c(1200, 800), background = '#ffffff', solid = T)

# render_points(extent = raster::extent(mv_zoom), lat = c(37.16663, 37.16155, 37.18395, 37.30354), long = c(-108.47300, -108.46423, -108.48699, -108.44074), 
#               zscale=10, color = "white", heightmap = elmat)
# render_label(elmat,lat = 37.1666338, long = -108.4730013, extent = raster::extent(mv_zoom),
#              text = "Cliff Palace", textsize = 50, linewidth = 5, clear_previous = TRUE)
Sys.sleep(0.2)
render_snapshot('MV_3Di', clear = F)


rgl::rgl.close()





# Movie
filename_movie = "MV-3D.mp4"

base_map %>% 
  add_overlay(stream_layer, alphalayer = 0.8) %>% 
  # add_overlay(trails_layer2) %>%
  # add_overlay(generate_point_overlay(mesaverde_attractions, extent = raster::extent(mv_zoom),
  #                                    size = 2, color = "black", heightmap = elmat)) %>%
  # add_overlay(generate_point_overlay(mesaverde_attractions, extent = raster::extent(mv_zoom),
  #                                    size = 3, color = "white", heightmap = elmat)) %>%
  plot_3d(elmat, zscale = 6)

render_movie(filename = filename_movie, phi = 45, 
            title_text = "Mesa Verde National Park")
