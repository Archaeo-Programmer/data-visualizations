library(tidyverse)
library(raster)
library(sf)
library(FedData)
library(purrr)
library(xts)
library(dygraphs)

ned_swcol <-
  raster::raster("../03-raster-timelapse/EXTRACTIONS/ned_SWCol/NED/ned_SWCol_NED_1.tif")

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
# dygraph(prcp.xts) %>% dyRangeSelector()

b <- js2graphic::svgFromHtml("precipitation_swcol.html")
