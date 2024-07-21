#install.packages("tidyverse")
library(ggraph)
library(igraph)
library(tidyverse)

rm(list=ls())
set.seed(100)

setwd("C:/Users/Raunak/OneDrive/Desktop/Master Thesis/src")


# Simulation Tree Structure -----------------------------------------------



# create an edge list data frame giving the hierarchical structure of your individuals
d1 <- data.frame(from="Root", to=paste("Group", seq(0,1), sep="_"))
d2 <- data.frame(from=rep(d1$to, each=3), to=paste("Subgroup", seq(2,4), sep="_"))
edges <- rbind(d1, d2)

#Nodes
name <- unique(c(as.character(edges$from)[2:8], as.character(edges$to)[3:8]))
vertices <- data.frame(
  name=name)

# Create a graph object 
mygraph <- graph_from_data_frame( edges, vertices = vertices )

# Basic tree
sim_plot = ggraph(mygraph, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_link() +
  geom_node_text(aes( label=name, filter = leaf) , angle= 90 , hjust=1, nudge_y = -0.04) +
  geom_node_text(aes(label = name, filter = !leaf), nudge_x = 0.2)+
  geom_node_point() +
  ylim(-.5, NA)+
  theme_void()
plot(sim_plot)

png(file="C:/Users/Raunak/OneDrive/Desktop/Master Thesis/in/sim_plot.png",
    width=700, height=480, pointsize = 10)
plot(sim_plot)
dev.off()


# Empirical Tree Structure ------------------------------------------------


# create an edge list data frame giving the hierarchical structure of your individuals
d3 <- data.frame(from=rep("Root",5),to = c("Subcompact","Compact","Intermediate","Standrad","Luxury"))
d4 <- data.frame(from=rep(d3$to, each=2), to=c("Domestic","Foreign"))
edges_emp <- rbind(d3, d4)

#Nodes
name_emp <- unique(c(as.character(edges_emp$from), as.character(edges_emp$to)))
vertices_emp <- data.frame(
  name=name_emp)

# Create a graph object 
graph_emp <- graph_from_data_frame( edges_emp, vertices = vertices_emp)

# Basic tree
emp_plot = ggraph(graph_emp, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_link() +
  geom_node_text(aes( label=name, filter = leaf) , angle= 90 , hjust=1, nudge_y = -0.04) +
  geom_node_text(aes(label = name, filter = !leaf), nudge_x = 0.5,nudge_y = -0.02)+
  geom_node_point() +
  ylim(-.5, NA)+
  theme_void()
plot(emp_plot)

png(file="C:/Users/Raunak/OneDrive/Desktop/Master Thesis/in/emp_plot.png",
    width=700, height=480, pointsize = 10)
plot(emp_plot)
dev.off()

