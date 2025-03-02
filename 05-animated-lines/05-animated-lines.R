library(tidyverse)
library(gganimate)
library(viridis)
library(hrbrthemes)

# Data is from Kohler et al. 2017 (available at: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5714260/bin/NIHMS74617-supplement-Supplementary_table_2.xls).
inequality <- readxl::read_xls("Kohler_et_al_2017_inequality.xls")

plot <- inequality %>%
  ggplot(aes(
    x = Date_AD,
    y = Gini,
    group = Big_Region,
    color = Big_Region
  )) +
  geom_line() +
  geom_point() +
  scale_color_viridis(discrete = TRUE, name = "Region") +
  ggtitle("Ancient Wealth Inequality Across the World") +
  theme_ipsum(
    axis_title_size = 12,
    axis_title_just = "center",
    plot_title_margin = 30
  ) +
  ylab("Gini Coefficient") +
  xlab("Year BC/AD") +
  transition_reveal(Date_AD)

animate(plot, height = 6, width = 8, units = "in", res = 150)

anim_save("regions_animated_inequality.gif")



plot <- inequality %>%
  ggplot(aes(
    x = Date_AD,
    y = Gini,
    group = World,
    color = World
  )) +
  geom_line() +
  geom_point() +
  scale_color_discrete(name = "World") +
  ggtitle("Ancient Wealth Inequality in the New and Old World") +
  theme_ipsum(
    axis_title_size = 12,
    axis_title_just = "center",
    plot_title_margin = 30
  ) +
  ylab("Gini Coefficient") +
  xlab("Year BC/AD") +
  transition_reveal(Date_AD)

animate(plot, height = 6, width = 8, units = "in", res = 150)

anim_save("world_animated_inequality.gif")




plot <- inequality %>%
  dplyr::rowwise() %>% 
  dplyr::mutate(delta_year = Date_AD - LocalNeo) %>% 
  ggplot(aes(
    x = delta_year,
    y = Gini,
    group = World,
    color = World
  )) +
  geom_line() +
  geom_point() +
  scale_color_discrete(name = "World") +
  ggtitle("Ancient Wealth Inequality in the New and Old World") +
  theme_ipsum(
    axis_title_size = 12,
    axis_title_just = "center",
    plot_title_margin = 30
  ) +
  ylab("Gini Coefficient") +
  xlab("Δ Years") +
  transition_reveal(delta_year)

animate(plot, height = 6, width = 8, units = "in", res = 150)

anim_save("world_animated_inequality_deltaYears.gif")

