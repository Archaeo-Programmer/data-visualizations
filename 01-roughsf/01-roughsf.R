# remotes::install_github("schochastics/roughsf")
library(roughsf)
library(tidyverse)
library(rnaturalearth)
library(sf)
library(ggtext)
library(maps)

# Modern pollen data, which was originally downloaded from the Neotoma Paleoecology Database using the neotoma R package.
dat <- readRDS("./01-roughsf/modern_pollen.RDS")

# Read in the United States data using the maps package. Then, transform the data to the crs = 4326 projection.
usa <- sf::st_as_sf(maps::map("state", fill = TRUE, plot = FALSE))
usa <- sf::st_transform(usa, crs = 4326)

# Assign a range of values to the states, which will be used to assign colors.
usa2 <- sf::st_cast(usa, "POLYGON") %>%
  mutate(mapcolor7 = sample(1:7, n(), replace = TRUE))

# Now, create a fill layer with a range of colors.
usa2$fill <- as.factor(usa2$ID)
usa2$fill <- c("#F9D8D6", "#CDF5F6", "#F9EBDF", "#EFD2AC", "#f9d6f7", "#d6f9d8", "#f7f9d6")[usa2$mapcolor7]
usa2$stroke <- 2
usa2$fillweight <- 4.0

# MULTIPOLYGON (and also MULTILINESTRING) are not supported
usa2 <- sf::st_cast(usa2, "POLYGON")

# Ensure that the modern pollen data (i.e., dat) is an sf object.
dat <- sf::st_sf(dat)
sf::st_crs(dat) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Set the size of the points and the color.
dat$size <- 8
dat$color <- "#000000"


# Now, adjust the size of the points based on the number of pine pollen grains at each site.
dat1 <- dat
dat1$size <- ambient::normalise(dat1$PINUSX)
dat1$radius <- dat1$size / 10
sf::st_crs(dat1) <- sf::st_crs(usa)
dat1$size <- ambient::normalise(dat1$size, to = c(10, 30))
sf::st_crs(dat1) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
dat1$color <- "red"

# Create a roughsf object.
roughsf_map <- roughsf::roughsf(list(usa2, dat1),
                                title = "Modern Pine Pollen Samples across the United States", caption = "*Points are scaled based on the count of pine pollen grains at each site. Data are from the Neotoma Paleoecology Database.",
                                title_font = "48px Pristina", font = "30px Pristina", caption_font = "18px Pristina",
                                roughness = 1, bowing = 1, simplification = 1,
                                width = 1600, height = 1000,
)

# Save the plot.
roughsf::save_roughsf(roughsf_map, "./01-roughsf/roughsf_map.png")
