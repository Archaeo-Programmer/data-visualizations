if (!require("pacman"))
  install.packages("pacman")
pacman::p_load(tidyverse, gganimate, zoo, ggthemes)
pacman::p_load_gh("hrbrmstr/ggchicklet")

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

# color palette for categories
m.color <- c(
  "Cash"    = "#1F4C5B",
  "Crypto"  = "#418C74",
  "Commodities"   = "#C8904D",
  `Real Estate` = "#767171",
  "Bonds"   = "#9E5050",
  "Stocks" = "#61425D"
)

barchart <- df %>%
  dplyr::mutate(asset_class = forcats::fct(asset_class)) %>%
  ggplot(., aes(x = asset_class, y = allocation, fill = asset_class)) +
  ggchicklet::geom_chicklet(radius = grid::unit(3, "mm"), color = "transparent") +
  ggthemes::theme_hc(base_family = "sans", style = 'darkunica') +
  coord_flip() +
  geom_label(aes(label = I(sprintf(
    "%.1f%%", allocation
  )), hjust = 0), fill = 'white') +
  scale_fill_manual(values = m.color) +
  scale_y_continuous(
    labels = function(x)
      scales::percent(x, scale = 1)
  ) +
  transition_states(moyr, transition_length = 10, state_length = 1) +
  labs(
    title = "Investment Portfolio Allocation Trends (2014-2023)",
    subtitle = '{closest_state}',
    fill = "asset_class",
    caption = 'Source: AGB Investment Firm (simulated dataset)',
    x = "Asset Class",
    y = "Allocation (%)"
  ) +
  theme(
    legend.position = "none",
    text = element_text(colour = "white"),
    axis.title.x = element_text(
      margin = unit(c(5, 5, 5, 5), "mm"),
      size = 14,
      face = "bold",
      colour = "white"
    ),
    axis.title.y = element_text(
      margin = unit(c(5, 5, 5, 5), "mm"),
      size = 14,
      face = "bold",
      colour = "white"
    ),
    axis.text = element_text(size = 12, colour = "white"),
    plot.title = element_text(
      size = 17,
      face = "bold",
      colour = "white"
    ),
    plot.caption = element_text(hjust = 0, colour = "white")
  )


# For saving the animation (though you can change the size, resolution, etc.)
anim_barchart <- animate(
  barchart,
  fps = 40,
  end_pause = 10,
  duration = 30,
  ease_aes("cubic-in"),
  width = 7.8,
  height = 6.35,
  units = "in",
  res = 300
)
anim_save(animation = anim_barchart, filename = "animated_barchart.gif")


# Percent change from previous wave
bar_sassy_perc_change <- shares.long %>%
  dplyr::mutate(wave = zoo::as.yearmon(wave, format = "%b %Y")) %>%
  dplyr::arrange(wave) %>%
  dplyr::mutate(wave = as.factor(wave)) %>%
  dplyr::mutate(wave_name = as.factor(format(as.yearmon(wave), "%B %Y")), perc = paste0(prop, "%")) %>%
  dplyr::group_by(segment) %>%
  mutate(perc_change = prop - lag(prop), rn = row_number()) %>%
  ungroup %>%
  dplyr::mutate(year = readr::parse_number(as.character(wave_name))) %>%
  # Make a ggplot, but add frame=year: one image per year
  ggplot(., aes(
    x = forcats::fct_rev(segment),
    y = perc_change,
    fill = segment
  )) +
  #geom_bar(stat = 'identity') +
  geom_chicklet(radius = grid::unit(3, "mm"),
                aes(group = segment),
                color = "transparent") +
  #theme_bw() +
  theme_hc(base_family = "sans", style = 'darkunica') +
  #theme_economist() +
  coord_flip() +
  #geom_label(aes(label = I(perc_change), hjust=0), fill = 'white') +
  scale_y_continuous(
    labels = function(x)
      scales::percent(x, scale = 1)
  ) +
  scale_fill_manual(values = m.color) +
  # gganimate specific bits:
  transition_states(wave_name,
                    transition_length = 5,
                    state_length = 1) +
  # transition_time(rn) +
  labs(
    title = "Global Warming’s Six Americas",
    subtitle = '{closest_state}',
    fill = "Segment",
    caption = 'Source: Yale Program on Climate Change Communication;\nGeorge Mason University Center for Climate Change Communication'
  ) +
  xlab("") +
  ylab("Percent Change in U.S. Adult Population") +
  theme(
    legend.position = "none",
    text = element_text(colour = "white"),
    axis.title.x = element_text(
      margin = unit(c(5, 10, 10, 10), "mm"),
      size = 14,
      face = "bold",
      colour = "white"
    ),
    axis.text = element_text(size = 12, colour = "white"),
    axis.title = element_text(
      size = 14,
      face = "bold",
      colour = "white"
    ),
    plot.title = element_text(
      size = 20,
      face = "bold",
      colour = "white"
    ),
    plot.caption = element_text(hjust = 0, colour = "white")
  )

# For saving the animation (though you can change the size, resolution, etc.)
anim_bar_sassy_perc_change <- animate(
  bar_sassy_perc_change,
  fps = 35,
  end_pause = 10,
  duration = 10,
  ease_aes("cubic-in"),
  width = 7.8,
  height = 6.35,
  units = "in",
  res = 300
)
anim_save(animation = anim_bar_sassy_perc_change, filename = "SASSY_trends_barchart_anim_dark_percChange.gif")




