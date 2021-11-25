library(tidyverse)
library(raster)
library(sf)
library(FedData)
library(purrr)
library(ggplot2)
library(magick)

set.seed(345)

# Bounding box for study area.
bb <-
  c(
    "xmin" = -109.0565,
    "xmax" = -108.2819,
    "ymin" = 37.03155,
    "ymax" = 37.66463
  ) %>%
  sf::st_bbox() %>%
  sf::st_as_sfc() %>%
  sf::st_as_sf(crs = 4326) %>%
  sf::st_transform(crs = 4326)

temperature <- raster::raster("modern_temperature.tif")

# Create some new raster layers.
temp_list <- replicate(20, temperature) %>% 
  # Change each layer by adding or subtracting a random value from a given list.
  purrr::map(., function(x){x + sample(c(0, 2, 4, 6, -2, -4, -6), 1)})

# Rename the layers (TS is Time Step).
names(temp_list) <- c(paste0("TS", 1:20))

# Define custom color palette.
colors <- colorRampPalette(rev(RColorBrewer::brewer.pal(11,"RdBu")))(255)

# Get elevation data for southwestern Colorado using the bounding box extent.
# ned_SWCol <- FedData::get_ned(template = bb, label = "ned_SWCol", 
#                     res="1", force.redo = F)
ned_SWCol <- raster::raster("./EXTRACTIONS/ned_SWCol/NED/ned_SWCol_NED_1.tif")

# Put elevation and temperature rasters in the same resolution.
ned_SWCol <- raster::projectRaster(ned_SWCol, temperature, method = 'ngb')

# Create a hillshade file.
slope <- raster::terrain(ned_SWCol, opt='slope')
aspect <- raster::terrain(ned_SWCol, opt='aspect')
hill <- raster::hillShade(slope, aspect,
                          angle = 40,
                          direction = 270)
hill_spdf <- as(hill, "SpatialPixelsDataFrame")
hill_spdf <- as.data.frame(hill_spdf)
colnames(hill_spdf) <- c("value", "x", "y")

# Create a ggplot function for each raster layer.
plot_temp_raster <-
  function(x, y, hillshade, animation = TRUE) {
    # Convert the raster to a dataframe for plotting in ggplot.
    x <- as.data.frame(x, xy = TRUE)
    
    plots <- ggplot() +
      ggplot2::geom_raster(
        data = hillshade,
        mapping = aes(x = x,
                      y = y,
                      alpha = value),
        na.rm = TRUE
      ) +
      # use the "alpha hack"
      scale_alpha(
        name = "",
        range = c(0.8, 0),
        na.value = 0,
        guide = "none"
      )  +
      geom_raster(
        data = x,
        aes(x = x, y = y, fill = modern_temperature),
        alpha = 0.5,
        na.rm = TRUE
      ) +
      scale_fill_gradientn(
        colors = colorRampPalette(rev(RColorBrewer::brewer.pal(11, "RdBu")))(255),
        limits = c(8 , 34),
        na.value = "transparent",
        name = "Temperature °C"
      ) +
      coord_equal() +
      theme_classic() +
      xlab("Longitude") +
      ylab("Latitude") +
      ggtitle(y) +
      theme(
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        axis.text = element_text(
          size = 12,
          colour = "black",
          family = "Helvetica"
        ),
        axis.title.y = element_text(
          size = 14,
          family = "Helvetica",
          margin = margin(
            t = 10,
            r = 5,
            b = 10,
            l = 10
          )
        ),
        axis.title.x = element_text(
          size = 14,
          family = "Helvetica",
          margin = margin(
            t = 5,
            r = 10,
            b = 10,
            l = 10
          )
        ),
        plot.title = element_text(
          hjust = 0.5,
          size = 18,
          family = "Helvetica"
        ),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.text = element_text(size = 12, family = "Helvetica"),
        legend.title = element_text(size = 12, family = "Helvetica")
      )
    
    if (animation == TRUE) {
      fp <-
        file.path("./animation",
                  paste0(y, ".png"))
      
      ggsave(
        plot = plots,
        filename = fp,
        width = 8.5,
        height = 5,
        dpi = 400,
        units = "in",
        device = "png"
      )
    }
    
    return(plots)
    
  }


# Create ggplot objects for each raster layer.
temperature_plots <-
  purrr::map2(
    .x = temp_list,
    .y = names(temp_list),
    .f =
      ~ plot_temp_raster(
        x = .x,
        y = .y,
        hillshade = hill_spdf,
        animation = TRUE
      )
  )

# Do quick animation with raster::animate.
tavg_animated <- raster::animate(tavg_rbrick, n =2, col = colors)


# Create a gif animation of the raster layers.
# List file names and read in files.
imgs <- list.files("./animation", full.names = TRUE)

# Put them in the correct order.
bfile.names <- sub("\\..*$", "", basename(imgs))
ts.order <- match(bfile.names, c(paste0("TS", 1:20)))
imgs <- imgs[ order(ts.order) ]

# Turn all images into a magick image.
img_list <- purrr::map(imgs, magick::image_read)

# Join the images together.
img_joined <- magick::image_join(img_list)

# Animate at 2 frames per second
img_animated <- magick::image_animate(img_joined, fps = 2)

# Save to disk
magick::image_write(image = img_animated,
                    path = "./animated_temperature.gif")

