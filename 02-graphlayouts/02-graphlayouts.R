#install.packages("graphlayouts")
library(igraph)
library(ggraph)
library(graphlayouts)
library(dplyr)
library(ggplot2)


# First, create a random network.
set.seed(413)
# Create a network with group structure
g <- igraph::sample_islands(4, 52, 0.6, 22)
g <- igraph::simplify(g)
# Adding the additional 9, as the list needs to be as long as the vertices of the graph (i.e., v())
V(g)$grp <- as.character(c(rep(1:9, each = 23), 9))

# The backbone layout allows for a little more structure when viewing a network hairball.
ggraph(g, layout = "stress") +
  geom_edge_link0(colour = rgb(0, 0, 0, 0.5), width = 0.1) +
  geom_node_point(aes(col = grp)) +
  scale_color_brewer(palette = "Set1") +
  theme_graph() +
  theme(legend.position = "none")

# Save image to file.
ggsave("ggraph_net_stress.png")


# Now, we can also use the backbone layout to discover to see possible groupings in the data.
bb <- layout_as_backbone(g, keep = 0.45)
E(g)$col <- F
E(g)$col[bb$backbone] <- T

ggraph(g,
       layout = "manual",
       x = bb$xy[, 1],
       y = bb$xy[, 2]) +
  geom_edge_link0(aes(col = col), width = 0.1) +
  geom_node_point(aes(col = grp)) +
  scale_color_brewer(palette = "Set1") +
  scale_edge_color_manual(values = c(rgb(0, 0, 0, 0.3), rgb(0, 0, 0, 1))) +
  theme_graph() +
  theme(legend.position = "none")

# Save image to file.
ggsave("ggraph_net_manual.png")


# You can also view networks in other ways, such as by heatmaps.
g.mat <- get.adjacency(g, sparse = FALSE)

# Change to long format (i.e., an edgelist but including all the 0s).
longData <- reshape2::melt(g.mat) %>%
  tidygraph::as_tibble(longData) %>%
  dplyr::mutate(Var1 = forcats::fct_rev(factor(Var1)))

# using geom_tile
cols <- RColorBrewer::brewer.pal(3, 'Blues')[c(1, 3)]

ggplot(longData, aes(x = Var2, y = Var1)) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradient(low = cols[1], high = cols[2]) +
  theme_bw() +
  ggtitle("Heat Map Representation of a Random Network") +
  xlab("") +
  ylab("") + #set clean background and no titles
  guides(fill = guide_colourbar(barheight = 12)) + # can set the length of colour bar
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  scale_y_discrete(breaks = seq(0, 200, 25)) +
  coord_fixed() # fix to square coordinator

# Save image to file.
ggsave("net_heatmap.png")

# Create a network map with color, curved lines, and groupings. Then, save image.
edge.start <- ends(g, es = E(g), names = F)[, 1]
edge.col <- V(g)$grp[edge.start]
plot.new()

png("curved_color_net.png")

curved_net <- plot(
  g,
  edge.color = edge.col,
  edge.curved = .1,
  vertex.color = V(g)$grp,
  vertex.label = NA,
  vertex.label.color = "black",
  vertex.label.cex = .7,
  main = "Random Network"
)

print(curved_net)
dev.off()


# library(threejs)
# g$layout <- graphlayouts::layout_with_stress3D(g)
# graphjs(g, bg = "black")
