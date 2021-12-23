# This is R script is just a temporary workspace until move working code into it's own folder.
#---------------------
library(tidyverse)
library(rayshader)
library(raster)

# Downtown N bounding box.
bb <-
  c(
    "xmin" = -86.80200,
    "xmax" = -86.75629,
    "ymin" = 36.14633,
    "ymax" = 36.17447
  ) %>%
  sf::st_bbox() %>%
  sf::st_as_sfc() %>%
  sf::st_as_sf(crs = 4326) %>%
  sf::st_transform(crs = 4326)

# shape <- sf::st_read("MCRV_TN.shp")

# ned_TN <- FedData::get_ned(template = bb, label = "ned_TN",
#                     res="1", force.redo = F)

# TN <- raster::raster("~/Dropbox/WSU/data-visualizations/ned_TN_NED_1.tif")
PP <- raster("~/Dropbox/USGS_one_meter_x53y400_TN_Eastern_2_16_B16_Del1_2016.tif")
elevation_matrix <- raster_to_matrix(PP)

Nashville <-
  list(
    raster("USGS_one_meter_x52y401_TN_Eastern_2_16_B16_Del1_2016.tif"),
    raster("USGS_one_meter_x51y401_TN_Eastern_2_16_B16_Del1_2016.tif")
  )
Nashville <- do.call(merge, Nashville)
Nashville <- projectRaster(Nashville, TN)
Nashville <- crop(Nashville, bb)
elevation_matrix <- raster_to_matrix(Nashville)


# moundbottom <- raster::raster("USGS_NED_OPR_TN_Middle_B1_2018_1632653NE_TIFF_2019.tif")
# elevation_matrix <- raster_to_matrix(moundbottom)

elevation_matrix %>%
  sphere_shade() %>%
  add_shadow(ray_shade(elevation_matrix)) %>%
  add_shadow(ambient_shade(elevation_matrix)) %>%
  plot_3d(elevation_matrix, zscale=50)
render_snapshot(clear = TRUE)

elevation_matrix %>%
  sphere_shade(texture = "imhof1") %>%
  add_shadow(ray_shade(elevation_matrix)) %>%
  add_shadow(ambient_shade(elevation_matrix)) %>%
  add_water(detect_water(elevation_matrix)) %>%
  plot_3d(elevation_matrix, solid = TRUE, shadow = TRUE, zscale = 3)
render_snapshot('Nashville', clear = T)
rgl::rgl.close()


elevation_matrix %>%
  sphere_shade(texture = "bw") %>%
  add_shadow(ray_shade(elevation_matrix)) %>%
  add_shadow(ambient_shade(elevation_matrix)) %>%
  plot_3d(elevation_matrix, solid = TRUE, shadow = TRUE, water = TRUE, zscale = 3, waterheight = 0.1)
render_snapshot('Nashville', clear = T)
rgl::rgl.close()

# --------------------

# Rayshader


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

localtif <- raster::raster("./03-raster-timelapse/EXTRACTIONS/ned_SWCol/NED/ned_SWCol_NED_1.tif")

mv_zoom <- raster::crop(localtif, bb)

# dem <- raster::raster("/Volumes/DATA/NED/EXTRACTIONS/SKOPE_SWUS/rasters/NED_1.tif") %>% 
#   raster::crop(bb)
# 
# vep_crop <- raster::crop(vep, bb)

# Small area of MV bounding box.
# bb_small <-
#   c(
#     "xmin" = -108.47651,
#     "xmax" = -108.45231,
#     "ymin" = 37.14609,
#     "ymax" = 37.16982
#   ) %>%
#   sf::st_bbox() %>%
#   sf::st_as_sfc() %>%
#   sf::st_as_sf(crs = 4326) %>%
#   sf::st_transform(crs = 4326)


# Convert it to a matrix.
elmat <- rayshader::raster_to_matrix(mv_zoom)

elmat %>% 
  height_shade(texture = rev((grDevices::colorRampPalette(c("#6AA85B", "#D9CC9A", "#FFFFFF")))(256))) %>% 
  add_overlay(sphere_shade(elmat, texture = "desert", 
                           zscale=3, colorintensity = 4), alphalayer=0.4) %>% 
  plot_3d(elmat, zscale = 6, theta = 0, phi=45, zoom = .56, windowsize = c(1200, 800), background = '#ffffff', solid = T)

render_snapshot('day28_flatearth', clear = F)

elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  add_shadow(ray_shade(elmat, zscale = 3), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0) %>%
  plot_3d(elmat, zscale = 10, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))
Sys.sleep(0.2)
render_snapshot()

elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  add_shadow(ray_shade(elmat, zscale = 3), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0) %>% 
  plot_3d(elmat)
render_camera(fov = 0, theta = 60, zoom = 1, phi = 45)
render_scalebar(limits=2000,label_unit = "m",position = "W", color_first = "black")
render_compass(position = "E", color_n = "black")
render_snapshot()




# Other
# bryce <- dem
# 
# # desert <- colorRampPalette(c("#ffe3b3","#6a463a","#dbaf70","#9c9988","#c09c7c"))(4)
# 
# bryce_mat <- rayshader::raster_to_matrix(dem)
# bryce_small <- rayshader::resize_matrix(bryce_mat, 0.4)
# 
# bryce_small %>% 
#   height_shade(texture = rev((grDevices::colorRampPalette(c("#6AA85B", "#D9CC9A", "#FFFFFF")))(256))) %>% 
#   add_overlay(sphere_shade(bryce_small, texture = "desert", 
#                            zscale=3, colorintensity = 4), alphalayer=0.4) %>% 
#   add_shadow(lamb_shade(bryce_small,zscale=8), 0.6) %>% 
#   add_shadow(ambient_shade(bryce_small, zscale = 8), 0.4) %>% 
#   add_shadow(texture_shade(bryce_small,detail=8/10,contrast=9,brightness = 13), 0.3) %>%
#   plot_map()

# Extent of Mesa Verde National Park
# lat_range <- c(37.156469, 37.350311)
# long_range <- c(-108.6, -108.3)
# 
# convert_coords <- function(lat,long, from = CRS("+init=epsg:4326"), to) {
#   data = data.frame(long=long, lat=lat)
#   sp::coordinates(data) <- ~ long+lat
#   sp::proj4string(data) = from
#   #Convert to coordinate system specified by EPSG code
#   xy = data.frame(sp::spTransform(data, to))
#   colnames(xy) = c("x","y")
#   return(unlist(xy))
# }
# 
# utm_bbox <- convert_coords(lat = lat_range, long=long_range, to = crs(mv_zoom))
# 
# extent_zoomed <- extent(utm_bbox[1], utm_bbox[2], utm_bbox[3], utm_bbox[4])
# bryce_zoom <- crop(bryce, extent_zoomed)
# bryce_zoom_mat <- raster_to_matrix(bryce_zoom)


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
plot_map(base_map)

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
                                                 linewidth = 4, color="skyblue2", 
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

# base_map %>% 
#   add_overlay(stream_layer, alphalayer = 0.8) %>% 
#   plot_3d(elmat, windowsize=c(1200,800))
# render_camera(theta=240,  phi=30, zoom=0.3,  fov=60)
# render_snapshot()



watercolor = "#2a89b3"
maxcolor = "#e6dbc8"
mincolor = "#b6bba5"
contour_color = "#7d4911"



base_map %>% 
  add_overlay(stream_layer, alphalayer = 0.8) %>% 
  # add_overlay(trails_layer2) %>%
  add_overlay(generate_point_overlay(mesaverde_attractions, extent = raster::extent(mv_zoom),
                                     size = 2, color = "black", heightmap = elmat)) %>%
  # add_overlay(generate_point_overlay(mesaverde_attractions, extent = raster::extent(mv_zoom),
  #                                    size = 3, color = "white", heightmap = elmat)) %>%
  plot_3d(elmat, zscale = 6, theta = 195, phi=45, zoom = .56, windowsize = c(1200, 800), background = '#ffffff', solid = T)

# render_points(extent = raster::extent(mv_zoom), lat = c(37.16663, 37.16155, 37.18395, 37.30354), long = c(-108.47300, -108.46423, -108.48699, -108.44074), 
#               zscale=10, color = "white", heightmap = elmat)
# render_label(elmat,lat = 37.1666338, long = -108.4730013, extent = raster::extent(mv_zoom),
#              text = "Cliff Palace", textsize = 50, linewidth = 5, clear_previous = TRUE)
Sys.sleep(0.2)
render_snapshot('MV_3Dg', clear = F)


rgl::rgl.close()




temp <- c(5,7,6,4,8)
barplot(temp, col="#8CA9BE", main="#FFFFFF")

c("#6AA85B", "#D9CC9A", "#FFFFFF")



# OLD
osm_bbox = c(long_range[1],lat_range[1], long_range[2],lat_range[2])

bryce_highway = opq(osm_bbox) %>% 
  add_osm_feature("highway") %>% 
  osmdata_sf() 

bryce_lines = st_transform(bryce_highway$osm_lines, crs=crs(bryce))

ggplot(bryce_lines,aes(color=osm_id)) + 
  geom_sf() +
  theme(legend.position = "none") +
  labs(title = "Open Street Map `highway` attribute around Mesa Verde National Park")

base_map %>% 
  add_overlay(generate_line_overlay(bryce_lines,extent = extent_zoomed,
                                    linewidth = 2, color="white",
                                    heightmap = bryce_zoom_mat)) %>% 
  plot_map()

bryce_trails = bryce_lines %>% 
  filter(highway %in% c("path","bridleway"))

bryce_footpaths = bryce_lines %>% 
  filter(highway %in% c("footway"))

bryce_roads = bryce_lines %>% 
  filter(!highway %in% c("path","bridleway", "footway"))


trails_layer <- generate_line_overlay(bryce_footpaths,extent = extent_zoomed,
                                     linewidth = 4, color="black", 
                                     heightmap = bryce_zoom_mat) %>% 
  add_overlay(generate_line_overlay(bryce_footpaths,extent = extent_zoomed,
                                    linewidth = 3, color="white",
                                    heightmap = bryce_zoom_mat)) %>%
  add_overlay(generate_line_overlay(bryce_trails,extent = extent_zoomed,
                                    linewidth = 2, color="black", lty=3, offset = c(2,-2),
                                    heightmap = bryce_zoom_mat)) %>%
  add_overlay(generate_line_overlay(bryce_trails,extent = extent_zoomed,
                                    linewidth = 2, color="white", lty=3,
                                    heightmap = bryce_zoom_mat)) %>%
  add_overlay(generate_line_overlay(bryce_roads,extent = extent_zoomed,
                                    linewidth = 4, color="grey30",
                                    heightmap = bryce_zoom_mat))


bryce_water_lines = opq(osm_bbox) %>% 
  add_osm_feature("waterway") %>% 
  osmdata_sf() 

bryce_streams = st_transform(bryce_water_lines$osm_lines,crs=crs(bryce)) 

bryce_rivers = bryce_streams %>% 
  filter(name %in% c("McElmo Creek", "Mancos River", "Mud Creek", "West Fork Mud Creek", "East Fork Mud Creek"))

stream_layer = generate_line_overlay(bryce_rivers,extent = extent_zoomed,
                                     linewidth = 4, color="skyblue2", 
                                     heightmap = bryce_zoom_mat)

base_map %>% 
  add_overlay(stream_layer, alphalayer = 0.8) %>% 
  add_overlay(trails_layer) %>%
  plot_map()

bryce_tourism_points = st_transform(bryce_tourism$osm_points,crs=crs(bryce))

bryce_attractions = bryce_tourism_points %>% 
  filter(tourism == "attraction") %>% 
  filter(name %in% c("Cliff Palace", "Balcony House", "Spruce Tree House", "The Knife Edge"))


base_map %>% 
  add_overlay(stream_layer, alphalayer = 0.8) %>% 
  add_overlay(trails_layer) %>%
  add_overlay(generate_label_overlay(bryce_attractions, extent = extent_zoomed,
                                     text_size = 2, point_size = 2, color = "white", 
                                     halo_color = "black", halo_expand = 10, 
                                     halo_blur = 20, halo_alpha = 0.8,
                                     seed=1, heightmap = bryce_zoom_mat, data_label_column = "name")) %>% 
  plot_map(title_text = "Mesa Verde National Park, Colorado", title_offset = c(15,15),
           title_bar_color = "grey5", title_color = "white", title_bar_alpha = 1)



# Park Map

watercolor = "#2a89b3"
maxcolor = "#e6dbc8"
mincolor = "#b6bba5"
contour_color = "#7d4911"

bryce_zoom_mat %>% 
  height_shade(texture = rev((grDevices::colorRampPalette(
    c("#6AA85B", "#D9CC9A", "#FFFFFF")
  ))(256))) %>% 
  add_shadow(lamb_shade(bryce_zoom_mat),0.2) %>% 
  add_overlay(generate_contour_overlay(bryce_zoom_mat, color = contour_color, 
                                       linewidth = 2, levels=seq(min(bryce_zoom_mat), max(bryce_zoom_mat), by=5)),alphalayer = 0.5) %>% 
  add_overlay(height_shade(bryce_zoom_mat,texture = "white"), alphalayer=0.35) %>% 
  add_overlay(generate_line_overlay(bryce_rivers,extent = extent_zoomed,
                                    linewidth = 3, color=watercolor, 
                                    heightmap = bryce_zoom_mat)) %>% 
  add_overlay(generate_line_overlay(bryce_footpaths,extent = extent_zoomed,
                                    linewidth = 3, color="black", 
                                    heightmap = bryce_zoom_mat)) %>% 
  add_overlay(generate_line_overlay(bryce_trails,extent = extent_zoomed,
                                    linewidth = 3, color="black", lty=2,
                                    heightmap = bryce_zoom_mat)) %>% 
  add_overlay(generate_line_overlay(bryce_roads,extent = extent_zoomed,
                                    linewidth = 6, color="black",
                                    heightmap = bryce_zoom_mat)) %>% 
  add_overlay(generate_line_overlay(bryce_roads,extent = extent_zoomed,
                                    linewidth = 5, color="white",
                                    heightmap = bryce_zoom_mat)) %>% 
  add_overlay(generate_label_overlay(bryce_attractions, extent = extent_zoomed,
                                     text_size = 2, point_size = 2, color = "black", 
                                     halo_color = "#e6e1db", halo_expand = 5, 
                                     halo_blur = 2, halo_alpha = 0.8,
                                     seed=1, heightmap = bryce_zoom_mat, 
                                     data_label_column = "name")) %>% 
  add_overlay(generate_label_overlay("Mancos River", x = -108.35877, y = 37.25772, extent = extent_zoomed,
                                     text_size = 2, point_size = 2, color = "black", 
                                     halo_color = "#e6e1db", halo_expand = 5, 
                                     halo_blur = 2, halo_alpha = 0.8,
                                     seed=1, heightmap = bryce_zoom_mat, 
                                     data_label_column = "name")) %>% 
  add_overlay(generate_scalebar_overlay(extent = extent_zoomed, length = 6000,
                                        text_color = "black",
                                        heightmap = montereybay, 
                                        latlong=TRUE, text_size = 1, 
                                        halo_color = "#e6e1db", halo_expand = 2, 
                                        halo_blur = 1, halo_alpha = 0.8)) %>%
  plot_map(title_text="Mesa Verde National Park, Colorado", title_color = "white",
           title_bar_alpha = 1, title_bar_color = "black")






