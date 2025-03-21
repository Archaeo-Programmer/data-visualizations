if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, gganimate, zoo, ggthemes, wesanderson)
pacman::p_load_gh("hrbrmstr/ggchicklet")

# Create a dataframe with population data.
df <- structure(list(age = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 
                       50, 55, 60, 65, 70, 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 
                       55, 60, 65, 70, 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 
                       60, 65, 70, 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 
                       65, 70, 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 
                       70, 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70), 
               sex = c("Male", "Male", "Male", "Male", "Male", "Male", 
                          "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male", 
                          "Male", "Female", "Female", "Female", "Female", "Female", 
                          "Female", "Female", "Female", "Female", "Female", "Female", 
                          "Female", "Female", "Female", "Female", "Male", "Male", "Male", 
                          "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male", 
                          "Male", "Male", "Male", "Male", "Female", "Female", "Female", 
                          "Female", "Female", "Female", "Female", "Female", "Female", 
                          "Female", "Female", "Female", "Female", "Female", "Female", 
                          "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male", 
                          "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Female", 
                          "Female", "Female", "Female", "Female", "Female", "Female", 
                          "Female", "Female", "Female", "Female", "Female", "Female", 
                          "Female", "Female"), 
               population = c(180, 160, 130, 140, 150, 160, 170, 90, 85, 80, 75, 70, 65, 
                              60, 40, 160, 150, 120, 130, 140, 150, 160, 80, 75, 70, 65, 
                              60, 55, 50, 30, 185, 165, 135, 148, 159, 166, 177, 99, 89, 
                              88, 74, 73, 68, 63, 43, 200, 190, 45, 77, 121, 131, 190, 
                              68, 89, 98, 54, 23, 68, 76, 23, 85, 65, 35, 48, 59, 66, 
                              177, 99, 89, 88, 54, 33, 88, 66, 23, 149, 120, 80, 70, 66, 
                              44, 57, 87, 71, 32, 96, 31, 29, 20, 11), 
               country = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                           1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
                           2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 
                           3, 3, 3, 3, 3, 3, 3, 3, 3)), 
               row.names = c(NA, -90L), 
               class = c("tbl_df", "tbl", "data.frame")) 

my.palette <- wes_palette("FrenchDispatch")[1:2]

pop <- ggplot(data = df, aes(
  x = as.factor(age),
  y = ifelse(sex == "Male",-population, population),
  fill = as.factor(sex)
)) +
  ggchicklet::geom_chicklet(radius = grid::unit(3, "mm"), color = "transparent", aes(group = as.factor(age))) +
  coord_flip() +
  xlab("Age") +
  ylab("Population") +
  scale_fill_manual(values = my.palette) +
  scale_y_continuous(labels = abs) +
  theme_economist_white(gray_bg = F) +
  gganimate::transition_states(country,
                    transition_length = 2,
                    state_length = 1) +
  labs(title = "Population by Country",
    subtitle = 'Country: {closest_state}', 
    fill = "Sex",
    color = "Sex") +
  ease_aes('linear') +
  theme(
    legend.position = "right",
    text = element_text(colour = "black"),
    axis.title.x = element_text(
      margin = unit(c(5, 5, 5, 5), "mm"),
      size = 14,
      face = "bold",
      colour = "black"
    ),
    axis.title.y = element_text(
      margin = unit(c(5, 5, 5, 5), "mm"),
      size = 14,
      face = "bold",
      colour = "black"
    ),
    axis.text = element_text(size = 12, colour = "black"),
    axis.title = element_text(
      size = 14,
      face = "bold",
      colour = "black"
    ),
    plot.title = element_text(
      size = 20,
      face = "bold",
      colour = "black"
    ),
    plot.caption = element_text(hjust = 0, colour = "black")
  )

# For saving the animation (though you can change the size, resolution, etc.)
anim_pop <- gganimate::animate(pop, height = 6, width = 6, units = "in", res = 300)
gganimate::anim_save(animation = anim_pop, filename ="animated-pyramid.gif")
