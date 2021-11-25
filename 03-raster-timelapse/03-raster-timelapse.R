library(tidyverse)
library(raster)
library(sf)
library(FedData)

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

temperature <- raster("modern_temperature.tif")

# Create some new raster layers.
temp_list <- replicate(20, temperature) %>% 
  # Change each layer by adding or subtracting a random value from a given list.
  purrr::map(., function(x){x + sample(c(0, 2, 4, 6, -2, -4, -6), 1)})

# Create a raster stack.
temp_rstack <- raster::stack(temp_list)
# Create a raster brick.
temp_rbrick <- raster::brick(temp_list)

# Rename the layers (TS is Time Step).
names(temp_rstack) <- c(paste0("TS", 1:20))

# Define custom color palette.
colors <- colorRampPalette(rev(RColorBrewer::brewer.pal(11,"RdBu")))(255)

# Get elevation data for southwestern Colorado using the bounding box extent.
ned_SWCol <- get_ned(template = bb, label = "ned_SWCol", 
                    res="1", force.redo = F)

# Create a hillshade file.
NED.raster.cropped2 <- raster::crop(NED, bbox_buffer2)

NED.raster.cropped2 <-  projectRaster(NED.raster.cropped2,tavg_preds_anom2_stdz[[1]],method = 'ngb')

vep3_ned2 <- raster("EXTRACTIONS/vep3/NED/vep3_NED_1.tif")
vep3_ned2 <- raster::aggregate(vep3_ned2, fact = 3)
vep3_ned2 <-  projectRaster(vep3_ned2,tavg_preds_anom2_stdz[[1]],method = 'ngb')

slope <- terrain(vep3_ned2, opt='slope')
aspect <- terrain(vep3_ned2, opt='aspect')
dsm.hill <- hillShade(slope, aspect,
                      angle=40,
                      direction=270)
hill_spdf <- as(dsm.hill, "SpatialPixelsDataFrame")
hill_spdf <- as.data.frame(hill_spdf)
colnames(hill_spdf) <- c("value", "x", "y")


names(tavg_preds_anom2_stdz) <- c("BC950","BC850","BC750", "BC650", "BC550","BC450", "BC350", "BC250", "BC150", "BC50", "AD50", "AD150", "AD250", "AD350", "AD450", "AD550", "AD650", "AD750", "AD850","AD950", "AD1050", "AD1150", "AD1250", "AD1350", "AD1450", "AD1550", "AD1650", "AD1750")

anomaly_plots2 <-
  purrr::map2(
    .x = tavg_preds_anom2_stdz, .y = names(tavg_preds_anom2_stdz),
    .f =
      ~plot_map_anomaly(x = .x, x.name = .y, us.cities = us.cities, hillshade = hill_spdf, cities = FALSE, animation = TRUE)
  )

# Arrange all of the plots, then can output below as PDF.
anomaly_plots_arranged <- gridExtra::marrangeGrob(anomaly_plots, nrow=2, ncol=1, top=NULL)

# Save as one pdf. Use scale here in order for the multi-plots to fit on each page.
ggsave("/Users/andrewgillreath-brown/Dropbox/WSU/SKOPEII/Figures/anomaly_plots_arranged_west.pdf", anomaly_plots_arranged, scale = 1.5,
       width = 210,
       height = 297,
       units = "mm")

transition_manual(date)

# Do quick animation with raster::animate.
tavg_animated <- raster::animate(tavg_rbrick, n =2, col = colors)


# Visualizing maize over the reconstructions.
tavg_rbrick3 <- tavg_rbrick[[1:15]]
usa <- map("state", fill = TRUE, plot = FALSE)
par(mfrow=c(5,3), mai = c(.2, .35, .3, .3))
for (ii in 1:nlayers(tavg_rbrick3)){
  plot(subset(tavg_rbrick3,ii), main=names(tavg_rbrick3)[ii])
  plot(MDB[[ii]], main=names(tavg_rbrick3)[ii], pch = 19, cex = 1, add=T)
  polygon(usa$x, usa$y)
}
dev.off()

#Gif animation
## list file names and read in
imgs <- list.files("/animation/", full.names = TRUE)

# Put them in the correct order.
bfile.names <- sub("\\..*$", "", basename(imgs))
year.names <-
  c("BC950","BC850","BC750", "BC650", "BC550","BC450", "BC350", "BC250", "BC150", "BC50", "AD50", "AD150", "AD250", "AD350", "AD450", "AD550", "AD650", "AD750", "AD850","AD950", "AD1050", "AD1150", "AD1250", "AD1350", "AD1450", "AD1550", "AD1650", "AD1750")
year.number <- match(bfile.names, year.names)
imgs <- imgs[ order(year.number) ]


# index = c(28:24, 22:19, 23, 13, 6, 10:12, 14:18, 1:5, 7:9)
# index = c(4, 3, 2, 1, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
# imgs = imgs[order(index)]
img_list <- lapply(imgs, magick::image_read)

## join the images together
img_joined <- magick::image_join(img_list)

## animate at 2 frames per second
img_animated2 <- magick::image_animate(img_joined, fps = 20)

## view animated image
img_animated

## save to disk
magick::image_write(image = img_animated2,
                    path = "/animated_temperature.gif")