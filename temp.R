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



library(raster)
localtif <- raster::raster("./03-raster-timelapse/EXTRACTIONS/ned_SWCol/NED/ned_SWCol_NED_1.tif")

# Convert it to a matrix.
elmat <- rayshader::raster_to_matrix(localtif)

# Plot with one of rayshader's textures:
elmat %>%
  sphere_shade(texture = "unicorn") %>%
  plot_map()


elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  add_shadow(ray_shade(elmat, zscale = 3), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0) %>%
  plot_3d(elmat, zscale = 10, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))
Sys.sleep(0.2)
render_snapshot()

render_camera(fov = 0, theta = 60, zoom = 1, phi = 45)
render_scalebar(limits=c(0, 15, 30),label_unit = "km",position = "W",
                scale_length = c(0.33,1), color_first = "black")
render_compass(position = "E", color_n = "black")
render_snapshot(clear=TRUE)




# Other
bryce <- localtif

desert <- colorRampPalette(c("#ffe3b3","#6a463a","#dbaf70","#9c9988","#c09c7c"))(4)

bryce_mat <- rayshader::raster_to_matrix(bryce)
bryce_small <- rayshader::resize_matrix(bryce_mat, 0.4)

bryce_small %>% 
  height_shade(texture = rev((grDevices::colorRampPalette(c("#6AA85B", "#D9CC9A", "#FFFFFF")))(256))) %>% 
  add_overlay(sphere_shade(bryce_small, texture = "desert", 
                           zscale=3, colorintensity = 4), alphalayer=0.4) %>% 
  add_shadow(lamb_shade(bryce_small,zscale=8), 0.6) %>% 
  add_shadow(ambient_shade(bryce_small, zscale = 8), 0.4) %>% 
  add_shadow(texture_shade(bryce_small,detail=8/10,contrast=9,brightness = 13), 0.3) %>%
  plot_map()

# Extent of Mesa Verde National Park
lat_range <- c(37.156469, 37.350311)
long_range <- c(-108.6, -108.3)

convert_coords <- function(lat,long, from = CRS("+init=epsg:4326"), to) {
  data = data.frame(long=long, lat=lat)
  coordinates(data) <- ~ long+lat
  proj4string(data) = from
  #Convert to coordinate system specified by EPSG code
  xy = data.frame(sp::spTransform(data, to))
  colnames(xy) = c("x","y")
  return(unlist(xy))
}

utm_bbox <- convert_coords(lat = lat_range, long=long_range, to = crs(bryce))

extent_zoomed <- extent(utm_bbox[1], utm_bbox[2], utm_bbox[3], utm_bbox[4])
bryce_zoom <- crop(bryce, extent_zoomed)
bryce_zoom_mat <- raster_to_matrix(bryce_zoom)


base_map <- bryce_zoom_mat %>%
  height_shade(texture = rev((grDevices::colorRampPalette(
    c("#6AA85B", "#D9CC9A", "#FFFFFF")
  ))(256))) %>%
  add_overlay(
    sphere_shade(
      bryce_small,
      texture = "desert",
      zscale = 3,
      colorintensity = 4
    ),
    alphalayer = 0.4
  ) %>%
  add_shadow(lamb_shade(bryce_zoom_mat, zscale = 8), 0.6) %>%
  add_shadow(ambient_shade(bryce_zoom_mat, zscale = 8), 0.4) %>%
  add_shadow(texture_shade(
    bryce_zoom_mat,
    detail = 8 / 10,
    contrast = 9,
    brightness = 13
  ),
  0.3)

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

