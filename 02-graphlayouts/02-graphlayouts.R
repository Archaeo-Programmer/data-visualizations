#install.packages("graphlayouts")

library(igraph)
library(ggraph)
library(graphlayouts)


set.seed(413)
# Create a network with group structure
g <- igraph::sample_islands(4,52,0.6,22)
g <- igraph::simplify(g)
# Adding the additional 9, as the list needs to be as long as the vertices of the graph (i.e., v())
V(g)$grp <- as.character(c(rep(1:9,each=23), 9))

# This allows for a little more structure in viewing the network hairball.
ggraph(g,layout = "stress") +
  geom_edge_link0(colour=rgb(0,0,0,0.5),width=0.1) +
  geom_node_point(aes(col=grp)) +
  scale_color_brewer(palette = "Set1") +
  theme_graph() +
  theme(legend.position = "none")

# Now, we can also use the backbone layout to discover some groupings.
bb <- layout_as_backbone(g,keep=0.45)
E(g)$col <- F
E(g)$col[bb$backbone] <- T

ggraph(g,layout="manual",x=bb$xy[,1],y=bb$xy[,2])+
  geom_edge_link0(aes(col=col),width=0.1)+
  geom_node_point(aes(col=grp))+
  scale_color_brewer(palette = "Set1")+
  scale_edge_color_manual(values=c(rgb(0,0,0,0.3),rgb(0,0,0,1)))+
  theme_graph()+
  theme(legend.position = "none")



# You can also view networks in other ways, such as heatmaps.
E(g)$weight <- c(rep(1:109,each=30), 9)
netm <- igraph::get.adjacency(g, attr="weight", sparse=F)
colnames(netm) <- V(net)$media
rownames(netm) <- V(net)$media

palf <- colorRampPalette(rev(RColorBrewer::brewer.pal(11,"RdYlBu")))
heatmap(netm[,208:1], Rowv = NA, Colv = NA, col = palf(200), labCol = (208:1),
        scale="none", margins=c(10,13) )
legend(x="right", legend=c("min edges", "medium edges", "max edges"),fill=palf(50))



edge.start <- ends(g, es=E(g), names=F)[,1]
edge.col <- V(g)$grp[edge.start]
plot.new()
plot(g, edge.color=edge.col, edge.curved=.1, vertex.color=V(g)$grp, vertex.label=V(g), vertex.label.color="black",
     vertex.label.cex=.7, main="Random Network")


# library(threejs)
# g$layout <- graphlayouts::layout_with_stress3D(g)
# graphjs(g, bg = "black")
