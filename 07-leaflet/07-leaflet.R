library(tidyverse)
library(raster)
library(sf)
library(FedData)
library(purrr)
library(xts)
library(dygraphs)
library(leaflet)

ned_swcol <-
  raster::raster("../03-raster-timelapse/EXTRACTIONS/ned_SWCol/NED/ned_SWCol_NED_1.tif") %>% 
  crop(bb)

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

# Download GHCN daily climate data. We are interested in
# precipitation (PRCP) but many other climate-related
# elements exist (i.e. max and min temp, snowfall, etc).
ghcn_swcol <- get_ghcn_daily(
  template = bb,
  label = "ghcn_swcol",
  elements = "prcp",
  force.redo = F
)


ghcn_pts <- data.frame(
  ID = ghcn_swcol$spatial$ID,
  lat = ghcn_swcol$spatial@coords[, 2],
  long = ghcn_swcol$spatial@coords[, 1]
)


ghcn_pts <- mutate(ghcn_pts, ID = as.character(as.factor(ID)))


ghcn_dat <- ghcn_swcol$tabular


# Pull out the PRCP (precipitation) tables. If there were other
# climate elements we could access these in a
# similar way (i.e. ghcn_dat, "[[", "TMIN")
prcp <- lapply(ghcn_dat, "[[", "PRCP")


# Dissolve the list into a single data.frame and
# then replace row names with a number sequence.
prcp <- do.call("rbind", prcp)
# prcp$station <- substring(row.names(prcp), 1, 11)
row.names(prcp) <- 1:nrow(prcp)

# Put into long format.
prcp <- prcp %>%
  tidyr::pivot_longer(!c(STATION, YEAR, MONTH),
                      names_to = "day",
                      values_to = "precip")


# Add the date
prcp <- dplyr::mutate(
  prcp,
  MONTH = stringr::str_pad(MONTH, 2, side = "left", pad = "0"),
  day = stringr::str_pad(gsub("D", "", day), 2,
                         side = "left", pad = "0"),
  date = as.Date(paste(YEAR, MONTH, day, sep = "-"))
)


# Exclude non NA values and limit to current years
prcp <- dplyr::filter(
  prcp,!is.na(date),!is.na(precip),
  date > as.Date("2011-01-01"),
  date < as.Date("2021-01-01")
)


# Do a count by station.
cnt <- prcp %>%
  dplyr::group_by(STATION) %>%
  dplyr::summarize(cnt = n()) %>% arrange(cnt) %>%
  dplyr::mutate(STATION = factor(STATION, levels = STATION))


# semi_join does a regular inner_join but only keeps the resulting
# columns from the left table. Greater than or equal to 2000 days
# looks like a good cutoff
prcp <-
  semi_join(prcp, dplyr::filter(cnt, cnt >= 2000), by = "STATION")

# Look at monthly data.
prcp <- dplyr::group_by(prcp, YEAR, MONTH, STATION) %>%
  dplyr::summarize(precip  = mean(precip, na.rm = T)) %>%
  dplyr::mutate(date = as.Date(paste(YEAR, MONTH, "15", sep = "-")))


# Make each column a station
prcp.wide <-
  spread(prcp[, c("date", "STATION", "precip")], STATION, precip)

# Now apply a 30-day moving average (both sides) to smooth out the variation
prcp.wide[, 2:ncol(prcp.wide)] <-
  lapply(prcp.wide[, 2:ncol(prcp.wide)],
         function(x)
           stats::filter(x, rep(1 / 30, 30, sides = 2)))


# Delete rows with no data and convert to a data.frame
howmanyNA <- rowSums(is.na(prcp.wide))
prcp.wide <- prcp.wide[howmanyNA != (ncol(prcp.wide) - 1), ]
prcp.wide <- data.frame(prcp.wide)

prcp.xts<-xts(prcp.wide[,-1], order.by = prcp.wide[,1])

dygraph(prcp.xts) %>%
  dyRangeSelector()

# Calculate annual averages and create a final data.frame of values
prcp.fin <- dplyr::filter(prcp, date > as.Date("2015-01-01")) %>%
  group_by(STATION) %>%
  summarize(prcpAvg = round(mean(precip, na.rm = T), 2)) %>%
  rename(ID = STATION)

prcp.fin <- left_join(prcp.fin, ghcn_pts, by = "ID")


# Create SpatialPointsDataFrame from GHCN points
pts <- data.frame(prcp.fin)
coordinates(pts) <-  ~ long + lat
proj4string(pts) <-
  "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
pts <- crop(pts, bb)

# Project the points and elevation raster to NY State Plane
swcolprj <-
  "+proj=lcc +lat_1=38.45 +lat_2=39.75 +lat_0=37.83333333333334 +lon_0=-105.5 +x_0=914401.8289 +y_0=304800.6096 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"
ptsprj <- spTransform(pts, CRS(swcolprj))
nedprj <- projectRaster(ned_swcol, crs = swcolprj)


# Extract elevation values at point locations
nedpts <- raster::extract(nedprj, ptsprj)


# Add elevation values to the points data and convert to feet
prcp.fin <- cbind(prcp.fin, nedpts)
prcp.fin <- mutate(prcp.fin, nedpts = round(nedpts / 0.3048, 2))

prcp.fin <- prcp.fin %>% 
  dplyr::filter(ID %in% c("USC00051886", "US1COMZ0034", "USC00055327", "USW00003061", "USC00055531"))


cols<-c("#06407F", "#317A9D", "#4ABEBB", "#40AE89", "#467B5D",
        "#3C6D4D", "#1A572E", "#034C00", "#045D03", "#6C975F", "#6B823A",
        "#88A237", "#C5D16B", "#DDE580", "#FFF6AE", "#FBCB81", "#F0B16A",
        "#F2B16D", "#D18338", "#B16F33", "#825337", "#66422A", "#4F2C0C")


pal <- colorNumeric(cols, values(nedprj),
                    na.color = "transparent")


leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
  addRasterImage(nedprj, colors = pal, opacity = 0.6) %>%
  addCircleMarkers(
    data = prcp.fin,
    lng = ~ long,
    lat = ~ lat,
    radius = ~ prcpAvg / 3,
    stroke = F,
    popup = paste(
      "<strong>Station ID</strong><br>",
      prcp.fin$ID,
      "<br><br><strong>Average Annual<br>Precipitation</strong><br>",
      prcp.fin$prcpAvg,
      " inches<br><br><strong>Elevation</strong><br>",
      prcp.fin$nedpts,
      " feet"
    ),
    color = "#006B8C",
    fillOpacity = 0.7
  )
