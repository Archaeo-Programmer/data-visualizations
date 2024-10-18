library(scholar)
#devtools::install_github('bbc/bbplot')
library(bbplot)
library(tidyverse)

# Here, using the scholar package to see network of co-authors from publications.
# You can change the number of coauthors, as well as how many authors extend from those co-authors.
# i.e., can be changed with n_deep.
# coauthors <- scholar::get_coauthors("1OebEyQAAAAJ", n_coauthors = 6, n_deep = 2)
# plot_coauthors(coauthors)


# Get number of publications and citations per year.

# Will need to clean up publications list so that I exclude things like blog posts, errors, etc.
venues <-
  c(
    "Anthropology Senior Thesis",
    "Ethnobiology Letters",
    "Fernvale",
    "Human Ecology",
    "Journal of Archaeological Science: Reports",
    "Journal of Ethnobiology",
    "Journal of Northwest Anthropology",
    "PAGES Magazine",
    "Paleobiology",
    "PloS one",
    "Santa Fe Institute",
    "The Cumberland River Archaic of Middle Tennessee",
    "University of North Texas"
  )

# Get number of publications per year for myself.
publications <- scholar::get_publications("1OebEyQAAAAJ") %>% 
  dplyr::mutate(journal = ifelse(str_detect(journal, fixed("Anthropology Senior Thesis")), "Anthropology Senior Thesis", journal)) %>% 
  #dplyr::filter(journal %in% venues) %>% 
  dplyr::filter(!is.na(year)) %>% 
  dplyr::select(year) %>% 
  dplyr::mutate(Publications = 1) %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(Publications = sum(Publications)) %>% 
  tidyr::complete(year = full_seq(year, 1), 
                  fill = list(Publications = 0))

# Get number of citations per year for myself, and join to publications dataframe.
citation <- scholar::get_citation_history("1OebEyQAAAAJ") %>% 
  dplyr::add_row(year = 2011, cites = 0) %>% 
  dplyr::arrange(year) %>% 
  # Fill in sequence for years with no citations.
  tidyr::complete(year = full_seq(year, 1), 
                  fill = list(cites = 0)) %>% 
  dplyr::left_join(., publications, by = "year") %>% 
  dplyr::rename(Citations = cites, Year = year) %>% 
  mutate(Publications = ifelse(is.na(Publications), 0, Publications))



# Here we plot and use the BBC News layout from bbplot.
p <- ggplot(citation, aes(Year)) +
  geom_bar(aes(y = Publications, fill = "Publications"), stat = "identity") +
  geom_line(aes(y = Citations, group = 1, color = "Citations"), size = 2) +
  scale_colour_manual(" ", values = c("Citations" = "#009E73")) +
  scale_fill_manual("", values = "#56B4E9") +
  bbc_style() +
  labs(title = "Number of Publications and Citations per Year") +
  theme(
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.box = "horizontal",
    plot.title = element_text(size = 20, hjust = 0.5)
  ) +
  scale_x_continuous(breaks = seq(2011, 2024, by = 2)) +
  scale_y_continuous(breaks = seq(0, 20, by = 2)) +
  ylab("Number of Publications") +
  xlab("Year")

# Save the plot and can add text to indicate the source data.
finalise_plot(plot_name = p,
              source = "Source: Google Scholar",
              save_filepath = "citations.png",
              width_pixels = 640,
              height_pixels = 550)

