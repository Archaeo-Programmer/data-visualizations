if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, gganimate, zoo, ggthemes, wesandersonviridis, hrbrthemes, tweenr, gifski)

library(reshape2)
library(readxl)
library(openxlsx)
library(transformr)
library(ggrepel)
library(dplyr)
library(png)
library(grid)
library(ggrepel)
library(readr)

# logo - example logo produced with ChatGPT
logo    <- suppressWarnings(readPNG(source = "logo.png"))
logo    <- rasterGrob(logo, interpolate = TRUE)

set.seed(123)

# Create a sequence of months over 10 years
months <- seq(as.Date("2014-01-01"), as.Date("2023-12-01"), by = "month")

# Number of months
n <- length(months)

# Simulating portfolio allocation changes over time
df <- data.frame(
  month = months,
  Stocks = abs(sin(seq(1, n, length.out = n)) * 50 + rnorm(n, 50, 5)),
  Bonds = abs(cos(seq(1, n, length.out = n)) * 30 + rnorm(n, 20, 3)),
  `Real Estate` = abs(sin(seq(1, n, length.out = n)) * 20 + rnorm(n, 15, 2)),
  Commodities = abs(cos(seq(1, n, length.out = n)) * 10 + rnorm(n, 10, 2)),
  Crypto = abs(runif(n, 0, 15)),
  Cash = abs(runif(n, 5, 15)),
  check.names = FALSE
) %>%
  dplyr::arrange(month) %>%
  # normalize to make proportions sum to 100% for each row
  dplyr::mutate(across(-month, ~ round(.x / rowSums(across(
    -month
  )) * 100)), moyr = forcats::fct_reorder(format(
    zoo::as.yearmon(month, format = "%B %Y"), "%B %Y"
  ), month)) %>%
  tidyr::pivot_longer(
    cols = -c(month, moyr),
    names_to = "asset_class",
    values_to = "allocation"
  ) %>%
  dplyr::mutate(perc = paste0(allocation, "%"))


rf.line <- df %>%
  dplyr::mutate(
    ease = 'sine-in-out',
    wave.text = as.character(moyr),
    wave = lubridate::year(month)
  )

tween.line <- tweenr::tween_elements(
  rf.line,
  time = 'wave',
  group = 'asset_class',
  ease = 'ease',
  nframes = 100
) %>%
  dplyr::mutate(.group = factor(
    .group,
    levels = c(
      "Stocks",
      "Bonds",
      "Real Estate",
      "Commodities",
      "Crypto",
      "Cash"
    )
  ), time = row_number())

# add line color parameter
tween.line <- tween.line %>%
  dplyr::mutate(
    linecol = dplyr::case_when(
      .group == 'Stocks' ~ 'blue',
      .group == 'Real Estate' ~ 'purple',
      .group %in% c("Bonds", "Commodities", "Crypto", "Cash") ~ 'gray'
    ) %>%
      as.factor()
  )

m.color <- c(
  'blue'       = "#1F4C5B",
  'gray'       = '#E6E5E5',
  'purple'     = '#61425D',
  'Stocks'    = "#1F4C5B",
  "Bonds"  = "#418C74",
  "Real Estate"   = "#C8904D",
  "Commodities" = "#767171",
  "Crypto"   = "#9E5050",
  "Cash" = "#61425D"
)

# identify the wave location
tween.line$wave <- as.character(tween.line$wave)

anim.line <- ggplot(tween.line, aes(time, allocation, group = .group)) +
  geom_line(aes(col = factor(linecol))) +
  geom_segment(aes(xend = 610, yend = allocation),
               linetype = 2,
               colour = 'grey') +
  geom_point(aes(size = allocation, col = factor(.group))) +
  geom_text_repel(
    aes(label = .group),
    x = 610,
    size = 5,
    direction = "y",
    force = 0.1
  ) +
  scale_color_manual(values = m.color) +
  scale_x_continuous(breaks = seq(1, 610, 56), 
                     labels=c("2014","2015","2016","2017","2018","2019","2020","2021", "2022", "2023", "2024"),
                     limits = c(1, 610)) +
  scale_y_continuous(breaks = seq(0, 70, 10), 
                     limits = c(0, 70)) +
  scale_size_continuous(range = c(2, 20)) +
  guides(size = "none", color = "none") +
  transition_reveal(time) +
  coord_cartesian(clip = 'off') +
  labs(title = 'Investment Portfolio Allocation Trends', 
       subtitle = "2014-2023",
       caption = 'Source: AGB Investment Firm (simulated dataset)',
       x = "Asset Class",
       y = "Allocation (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, hjust = 0),
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 15,
                                margin = unit(c(5, 5, 5, 5), "mm")),
    axis.title.x = element_text(size = 15,
                                margin = unit(c(5, 5, 5, 5), "mm")),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = unit(c(1, 2, 3, 1), 'cm'),
    panel.spacing = unit(c(2, 2, 2, 2), 'cm')
  ) +
  #annotation_custom(grob=text.url, xmin=365, ymin=-5, ymax=-4.5)+
  annotation_custom(
    logo,
    xmin = 50,
    xmax = 1125,
    ymax = -10,
    ymin = -23
  )

animate(
  anim.line,
  fps = 10,
  end_pause = 18,
  width = 7.8,
  height = 6.35,
  units = "in",
  res = 300
)
anim_save(filename = "animated-line-graph.gif", animation = last_animation())
