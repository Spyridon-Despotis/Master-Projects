
############ Project Info -----------------
# M.Sc. Program in Business Analytics @ AUEB
# Course: Social Network Analysis - Spring 2022
# Instructor: Katia Papakonstantinopoulou
# Project: "A Song of Ice and Fire Network"
# Student: Despotis Spyridon | P2822111


############ Sections -----------------
#0.Data Preperation
#1.Undirected Weighted Graph
#2.Network Properties
#3.Subgraph
#4.Centrality
#4.Ranking and Visualization


############ 0.Data Preparation -----------------
# In this section we prepare our data for the tasks later
# Install / Load the required Library to analyze the network
library(igraph)
#install.packages("plot3Drgl")
library(plot3Drgl)
library(tidyverse)
library(magrittr)

#Load the given dataset
df <- read.csv("asoiaf-all-edges.csv", header =T, sep=",")

#Selecting the appropriate columns
df1 = df[,c("Source","Target","weight")]

str(df)
#Checking the structure of our data
str(df1)

#Checking for missing values
sapply(df1, function(x) sum(is.na(x)))


############ Problem 1: Undirected Weighted Graph -----------------
# In this section we are trying
# To create an undirected weighted graph
# For the columns Source, Target, and Weight 

uwgraph <- graph_from_data_frame(df1, directed=F, vertices= NULL);uwgraph


# Checking if the graph is weighted
is.weighted(uwgraph) 

#Nodes
V(uwgraph)

#Names of each node
V(uwgraph)$name

#The edges between vertices
E(uwgraph)

#Plot the graph
plot(uwgraph,vertex.color="red",edge.color = "grey60")

#Another plot
plot(uwgraph, layout_with_lgl(uwgraph), vertex.label=NA)

#Another plot
plot(uwgraph,
     layout=layout_with_fr,
     vertex.color="grey80",
     vertex.label = NA, 
     edge.arrow.width=3, 
     edge.color = "orange",
     vertex.shape="circle",
     vertex.size= 8)

############ Problem 2: Network properties-----------------
##2.1 Number of vertices
vcount(uwgraph)

##2.2 Number of edges
gsize(uwgraph) 

##2.3 Diameter of the graph
diameter(uwgraph)

##2.4 Number of triangles
sum(count_triangles(uwgraph))

##or to find only the actual number of triangles without the revisions
sum(count_triangles(uwgraph)) / 3

##2.5 The top-10 characters of the network as far as their degree is concerned
head(sort(degree(uwgraph), decreasing=TRUE), n=10) 

##2.6 The top-10 characters of the network as far as their weighted degree is concerned
head(sort(strength(uwgraph), decreasing=TRUE), n=10)


############ Problem 3: Subgraph-----------------
##3.1 Plot the entire network (experiment with different parameters/layouts)
plot(uwgraph, layout=layout_nicely, vertex.color="green", vertex.label = NA, edge.arrow.width=10, edge.arrow.size=0.5, vertex.size=4)
plot(uwgraph, vertex.label = NA, layout=layout_with_fr, vertex.size=3, vertex.color="pink", edge.arrow.width=0.75)
plot(uwgraph, layout=layout_with_kk, vertex.color="purple", vertex.label = NA, edge.arrow.width=10, edge.arrow.size=0.5, vertex.size=3.5)

##3.2 Creating a Subgraph of the network by discarding all vertices 
##that have less than 10 connections in the network

#Subset
subset10<-which(degree(uwgraph) >9)
subgraph <- induced_subgraph(uwgraph, subset10)

#Plot Subgraph
#layout_with_graphopt
rglplot(subgraph, vertex.label = NA, layout= layout_with_graphopt, vertex.size=3, vertex.color="red", edge.arrow.width=0.75)

#layout_on_sphere
rglplot(subgraph, vertex.label = NA, layout=layout_on_sphere, vertex.size=3, vertex.color="red", edge.arrow.width=0.75)

#layout.fruchterman.reingold
rglplot(subgraph, vertex.label = NA, layout=layout.fruchterman.reingold(subgraph, niter=10), vertex.size=3, vertex.color="pink", edge.arrow.width=0.75)

#layout.kamada.kawai
plot.igraph(subgraph, 
            vertex.size = 3, 
            vertex.color = "red", 
            layout = layout.kamada.kawai, 
            vertex.label = NA, 
            edge.arrow.size = 0.8,
            edge.curved = 0.1,
            width = 0.9,
            main = "Subgraph of the network with >10 Connections",
            sub   = "A Song of Ice and Fire")

#layout_in_circle
plot.igraph(subgraph, 
            vertex.size = 3, 
            vertex.color = "red", 
            layout = layout_in_circle, 
            vertex.label = NA, 
            edge.arrow.size = 0.75,
            edge.curved = 0.1,
            width = 0.5,
            main = "Subgraph of the network with >10 Connections",
            sub   = "A Song of Ice and Fire")

#layout_as_tree
plot.igraph(subgraph, 
            vertex.size = 3, 
            vertex.color = "pink", 
            layout = layout_as_tree(subgraph, circular = TRUE),
            vertex.label = NA, 
            edge.arrow.size = .4,
            edge.curved = .1,
            width = .5,
            main = "Subgraph of the Network with >10 Connections",
            sub   = "A Song of Ice and Fire")


#"layout_with_fr
plot.igraph(subgraph, 
            vertex.size = 3, 
            vertex.color = "red", 
            layout = layout_with_fr, 
            vertex.label = NA, 
            edge.arrow.size = 0.75,
            edge.curved = 0.1,
            width = 0.5,
            main = "Subgraph of the network with >10 Connections",
            sub   = "A Song of Ice and Fire")



##3.3 Edge density for the entire graph
edge_density(uwgraph, loops = FALSE)

##3.4 Edge density for the subgraph
edge_density(subgraph, loops = FALSE)


############ Problem 4: Centrality-----------------
##4.1 Closeness Centrality
closeness <- uwgraph |>
  closeness() |>
  sort(decreasing = TRUE) |>
  head(n=15);closeness
#or head(sort(closeness(uwgraph), decreasing=T), n=15)

##4.2 Betweenness Centrality
betweenness <- uwgraph |>
    betweenness(directed = FALSE) |>
    sort(decreasing = TRUE) |>
    head(n=15);betweenness
#or head(sort(betweenness(uwgraph, directed=F), decreasing=T), n=15)

##4.3 Jon Snow Ranking
#Jon Snow Betweenness Centrality
sorted_names <- uwgraph |>
  betweenness(directed = FALSE) |>
  sort(decreasing = TRUE) |>
  names()
which(sorted_names == "Jon-Snow")

#Jon Snow Closeness Central
sorted_names <- uwgraph |>
  closeness() |>
  sort(decreasing = TRUE) |>
  names()
which(sorted_names == "Jon-Snow")


############ Problem 5: Ranking and Visualization-----------------
##5.1 Ranking

page_rank <- uwgraph |>
  page.rank()|>
  use_series("vector") |>
  sort(decreasing = T) |>
  as.matrix() |>
  set_colnames("page.rank") -> 
  pagerank
  as.data.frame(pagerank) -> 
  pagerank
  head(pagerank, n=10)

##5.2 Plot
# For plotting purposes the page rank of the characters are resized
subrk<- as.numeric(pagerank[,1] * 500)

plot(uwgraph,
     vertex.label = NA ,
     vertex.color = "orange",
     vertex.frame.color = "white",
     layout = layout_with_kk,
     vertex.size=subrk,
     edge.color = "grey60",
     edge.width = 0.5,
     edge.arrow.size = 0.5,
     edge.arrow.width = 0.5,
     edge.lty = c("solid"),
     edge.curved = 0.1,
     main = "Page Rank Metric")

plot(uwgraph,
     vertex.label = NA ,
     vertex.color = "orange",
     vertex.frame.color = "white",
     vertex.shape="vrectangle",
     layout = layout_on_sphere,
     vertex.size=subrk,
     edge.color = "grey60",
     edge.width = 0.5,
     edge.arrow.size = 0.5,
     edge.arrow.width = 0.5,
     edge.lty = c("solid"),
     edge.curved = 0.1)

# Taking specific degrees
subdegree<-which(degree(uwgraph) > 70)
GX <- induced_subgraph(uwgraph, subdegree)
GX |> page_rank(algo="prpack" , directed=FALSE) |> getElement("vector") -> pagerank2
pagerank2 <- page_rank(GX, algo="prpack", directed=FALSE)$vector
plot(GX, 
     layout=layout_nicely, 
     vertex.color = "grey80",
     vertex.frame.color = "white",
     vertex.label.family = "Arial",
     edge.color = "red",
     vertex.size=pagerank2*300,
     vertex.label.font = 4,
     vertex.label.cex = 0.8,
     edge.lty = c("solid"))

############################THE END #################################
