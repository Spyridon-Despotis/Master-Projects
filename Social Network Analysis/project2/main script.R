###############################
#SOCIAL NETWORK ANALYSIS
#Assignment 2
#Student: Despotis Spyridon
#Student ID: P2822111  
##############################

setwd("C:\\Users\\Spyros Despotis\\Desktop\\SNA 2022 FINAL")

################## Add libraries
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(igraph)
library(ggplot2)
library(magrittr)
library(reshape2)
library(insight)
library(visNetwork)
library(knitr)
library(corrplot)
library(corpcor)
library(RColorBrewer)

############## TASK 1 -DBLP co-authorship graph-

#reading the manipulated datasets
a2016 <- read.csv("2016.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)
a2017 <- read.csv("2017.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)
a2018 <- read.csv("2018.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)
a2019 <- read.csv("2019.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)
a2020 <- read.csv("2020.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)

############## Constructing Network
#creating the appropriate graphs
g_2016 <- graph.data.frame(a2016, directed=FALSE, vertices=NULL)
g_2017 <- graph.data.frame(a2017, directed=FALSE, vertices=NULL)
g_2018 <- graph.data.frame(a2018, directed=FALSE, vertices=NULL)
g_2019 <- graph.data.frame(a2019, directed=FALSE, vertices=NULL)
g_2020 <- graph.data.frame(a2020, directed=FALSE, vertices=NULL)

############## TASK 2-Average degree over time-

#create a variable of year
year <- c('2016','2017','2018','2019','2020')

#Merge all the igraphs into one list
time_graph <- list(g_2016,g_2017,g_2018,g_2019,g_2020)

#Number of Vertices for each year
gvertices <- c(vcount(g_2016),vcount(g_2017),vcount(g_2018),vcount(g_2019),vcount(g_2020))
gvertices <- data.frame(year,gvertices)
kable(gvertices,caption = "Number of nodes for each Year")

#Number of Edges for each year
gedges <- c(ecount(g_2016),ecount(g_2017),ecount(g_2018),ecount(g_2019),ecount(g_2020))
gedges <- data.frame(year, gedges)
kable(gedges,caption = "Number of Edges for each Year")

#Diameter of the graph for each year
gdiameter <- c(diameter(g_2016),diameter(g_2017),diameter(g_2018),diameter(g_2019),diameter(g_2020))
gdiameter <- data.frame(year,gdiameter)
kable(gdiameter,caption = "Diameter for each Year")

#Average degree for each year
gaverage_degree <- c(mean(degree(g_2016)),mean(degree(g_2017)),mean(degree(g_2018)),mean(degree(g_2019)),mean(degree(g_2020)))
gaverage_degree <- data.frame(year,gaverage_degree)
kable(gaverage_degree,caption = "Average Degree for each Year")

#Plot of vertices over time
pal = colorRampPalette(c("#1b98e0", "red"))
ggplot(gvertices, 
       aes(year, gvertices, color = gvertices, group = 1)) +
  geom_line(size = 3) +
  geom_point(size=5)+
  scale_colour_gradientn(colors=pal(100))+ 
  xlab("Year") + 
  ylab("Vertices") +
  labs(color='Vertices')+
  ggtitle("Vertices Over Time") +
  theme(plot.title = element_text(hjust = 0.5))

##################################
#Plot of edges over time
pal = colorRampPalette(c("#1b98e0", "pink"))
ggplot(gedges, 
       aes(year, gedges, color = gedges, group = 1)) +
  geom_line(size = 3) +
  geom_point(size=5)+
  scale_colour_gradientn(colors=pal(100))+ 
  xlab("Year") + 
  ylab("Edges") +
  labs(color='Edges')+
  ggtitle("Edges Over Time") +
  theme(plot.title = element_text(hjust = 0.5))

######################################
#Plot of diameter over time
pal = colorRampPalette(c("#1b98e0", "orange"))
ggplot(gdiameter, 
       aes(year, gdiameter, color = gdiameter, group = 1)) +
  geom_line(size = 3) +
  geom_point(size=5)+
  scale_colour_gradientn(colors=pal(100))+ 
  xlab("Year") + 
  ylab("Diameter") +
  labs(color='Diameter')+
  ggtitle("Diameter Over Time") +
  theme(plot.title = element_text(hjust = 0.5))

####################################
#Plot of average degree over time
pal = colorRampPalette(c("#1b98e0", "green"))
ggplot(gaverage_degree, 
       aes(year, gaverage_degree, color = gaverage_degree, group = 1)) +
  geom_line(size = 3) +
  geom_point(size=5)+
  scale_colour_gradientn(colors=pal(100))+ 
  xlab("Year") + 
  ylab("Average Degree") +
  labs(color='Average Degree')+
  ggtitle("Average Degree Over Time") +
  theme(plot.title = element_text(hjust = 0.5))

############## Task 3 -Important nodes-

#A. Top 10 authors based on degree by each year
degrees16 <- sort(degree(g_2016,V(g_2016),loops = T), decreasing = T)
degrees17 <- sort(degree(g_2017,V(g_2017),loops = T), decreasing = T)
degrees18 <- sort(degree(g_2018,V(g_2018),loops = T), decreasing = T)
degrees19 <- sort(degree(g_2019,V(g_2019),loops = T), decreasing = T)
degrees20 <- sort(degree(g_2020,V(g_2020),loops = T), decreasing = T)
top_degree <- data.frame(Network16 = names(head(degrees16,10)),
                         Network17 = names(head(degrees17,10)),
                         Network18 = names(head(degrees18,10)),
                         Network19 = names(head(degrees19,10)),
                         Network20 = names(head(degrees20,10)))
knitr::kable(top_degree, caption = "Top 10 nodes by Degree")

Network161 = (head(degrees16,10));Network161;
Network171 = (head(degrees17,10));Network171;
Network181 = (head(degrees18,10));Network181;
Network191 = (head(degrees19,10));Network191;
Network201 = (head(degrees20, 10));Network201;



#B. Top 10 authors based on pagerank by each year
pagerank16 <- sort(page.rank(g_2016)$vector, decreasing = T)
pagerank17 <- sort(page.rank(g_2017)$vector, decreasing = T)
pagerank18 <- sort(page.rank(g_2018)$vector, decreasing = T)
pagerank19 <- sort(page.rank(g_2019)$vector, decreasing = T)
pagerank20 <- sort(page.rank(g_2020)$vector, decreasing = T)
top_pagerank <- data.frame(Network160 = names(head(pagerank16,10)),
                           Network170 = names(head(pagerank17,10)),
                           Network180 = names(head(pagerank18,10)),
                           Network190 = names(head(pagerank19,10)),
                           Network200 = names(head(pagerank20,10)))
knitr::kable(top_pagerank, caption = "Top 10 nodes by PageRank")


Network160 = (head(pagerank16,10));Network160;
Network170 = (head(pagerank17,10));Network170;
Network180 = (head(pagerank18,10));Network180;
Network190 = (head(pagerank19,10));Network190;
Network200 = (head(pagerank20,10));Network200;



################### Task 4
#1.greedy clustering (hiearchical)
start.time1 <- Sys.time()
cfast_greedy_2016 <- cluster_fast_greedy(g_2016)
cfast_greedy_2017 <- cluster_fast_greedy(g_2017)
cfast_greedy_2018 <- cluster_fast_greedy(g_2018)
cfast_greedy_2019 <- cluster_fast_greedy(g_2019)
cfast_greedy_2020 <- cluster_fast_greedy(g_2020)
end.time <- Sys.time()
greedy.time.taken <- end.time - start.time1
greedy.time.taken

#2.infomap clustering
start.time2 <- Sys.time()
cinfomap_2016 <- cluster_infomap(g_2016)
cinfomap_2017 <- cluster_infomap(g_2017)
cinfomap_2018 <- cluster_infomap(g_2018)
cinfomap_2019 <- cluster_infomap(g_2019)
cinfomap_2020 <- cluster_infomap(g_2020)
end.time <- Sys.time()
infomap.time.taken <- end.time - start.time2
infomap.time.taken

#3. louvain clustering
start.time3 <- Sys.time()
clouvain_2016 <- cluster_louvain(g_2016)
clouvain_2017 <- cluster_louvain(g_2017)
clouvain_2018 <- cluster_louvain(g_2018)
clouvain_2019 <- cluster_louvain(g_2019)
clouvain_2020 <- cluster_louvain(g_2020)
end.time <- Sys.time()
louvain.time.taken <- end.time - start.time3
louvain.time.taken

#creating a table to compare the data
df2 <- data.frame(
  Year = c(2016, 2017,2018,2019, 2020),
  Fast_Greedy = c(round(modularity(cfast_greedy_2016),3), round(modularity(cfast_greedy_2017),3), round(modularity(cfast_greedy_2018),3), round(modularity(cfast_greedy_2019),3), round(modularity(cfast_greedy_2020),3)),
  Infomap = c(round(modularity(cinfomap_2016),3), round(modularity(cinfomap_2017),3), round(modularity(cinfomap_2018),3), round(modularity(cinfomap_2019),3), round(modularity(cinfomap_2020),3)),
  Louvain = c(round(modularity(clouvain_2016),3), round(modularity(clouvain_2017),3),round(modularity(clouvain_2018),3), round(modularity(clouvain_2019),3),round(modularity(clouvain_2020),3))
)
knitr::kable(df2, format = "markdown")

#plotting the results
long_df = melt(df2, id.vars=c("Year"))
long_df
ggplot(long_df, aes(x = Year, y = value, color = variable)) +
  geom_line()+
  geom_point() +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(
    x = "Year",
    title = "Comparison of Modularity Values",
    color = "Method"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


## Comparisons Between Infomap & Louvain
row1 <- c(1 - compare(cinfomap_2016,clouvain_2016),
          1 - compare(cinfomap_2017,clouvain_2017),
          1 - compare(cinfomap_2018,clouvain_2018),
          1 - compare(cinfomap_2019,clouvain_2019),
          1 - compare(cinfomap_2020,clouvain_2020))

## Comparisons Between Infomap & Fastgreedy
row2 <- c(1 - compare(cinfomap_2016,cfast_greedy_2016),
          1 - compare(cinfomap_2017,cfast_greedy_2017),
          1 - compare(cinfomap_2018,cfast_greedy_2018),
          1 - compare(cinfomap_2019,cfast_greedy_2019),
          1 - compare(cinfomap_2020,cfast_greedy_2020))


## Comparisons Between Louvain & Fastgreedy
row3 <- c(1 - compare(clouvain_2016,cfast_greedy_2016),
          1 - compare(clouvain_2017,cfast_greedy_2017),
          1 - compare(clouvain_2018,cfast_greedy_2018),
          1 - compare(clouvain_2019,cfast_greedy_2019),
          1 - compare(clouvain_2020,cfast_greedy_2020))
algo1 <- c("InfoMap","InfoMap","Clouvain")
algo2 <- c("Clouvain","Fast Greedy","Fast Greedy")
df <- data.frame(rbind(row1,row2,row3))
df <- cbind(algo1,algo2,df)
names(df) <- c("Algorithm 1","Algorithm 2","Year 2016","Year 2017","Year 2018",
               "Year 2019","Year 2020")
kable(df,caption = "Community Detection Algorithms similarity for each Year")

############# pICKING RANDOM AUTHOR
#common authors
commonauthors <- Reduce(intersect, 
                        list(
                          clouvain_2016$names,
                          clouvain_2016$names, 
                          clouvain_2017$names,
                          clouvain_2018$names,
                          clouvain_2019$names,
                          clouvain_2020$names))

## Add feature in the graph. 
V(g_2016)$k <- clouvain_2016$membership
V(g_2017)$k <- clouvain_2017$membership
V(g_2018)$k <- clouvain_2018$membership
V(g_2019)$k <- clouvain_2019$membership
V(g_2020)$k <- clouvain_2020$membership

#Picking random author
#author <- sample(commonauthors,1,replace = F)
author<-"W. Bruce Croft"

cn <- V(g_2016)[V(g_2016)$name==author]$k
g16 <- induced.subgraph(g_2016,
                        V(g_2016)[V(g_2016)$k==cn]$name)
plot(g16,
     vertex.size = 10,
     vertex.font.size = 8,
     layout = layout_components)

cn <- V(g_2017)[V(g_2017)$name==author]$k
g17 <- induced.subgraph(g_2017,
                        V(g_2017)[V(g_2017)$k==cn]$name) 
plot(g17,
     vertex.size = 10,
     vertex.font.size = 8,
     layout = layout_components)

cn <- V(g_2018)[V(g_2018)$name==author]$k
g18 <- induced.subgraph(g_2018,
                        V(g_2018)[V(g_2018)$k==cn]$name)
plot(g18,
     vertex.size = 10,
     vertex.font.size = 8,
     layout = layout_components)

cn <- V(g_2019)[V(g_2019)$name==author]$k
g19 <- induced.subgraph(g_2019,
                        V(g_2019)[V(g_2019)$k==cn]$name)
plot(g19,
     vertex.size = 10,
     vertex.font.size = 8,
     layout = layout_components)

cn <- V(g_2020)[V(g_2020)$name==author]$k
g20 <- induced.subgraph(g_2020,
                        V(g_2020)[V(g_2020)$k==cn]$name)
plot(g20,
     vertex.size = 10,
     vertex.font.size = 8,
     layout = layout_components)


############### Finding Similarities in communities
find_sim <- function(x,y) {
  length(intersect(V(x)$name,V(y)$name))/length(union(V(x)$name,V(y)$name))
}

#compute similarity
lst <- list(g16,g17,g18,g19,g20)
sim <- vector()
r = 1
for (i in 1:5) {
  for (j in 1:5) {
    sim[r] <- find_sim(lst[[i]],
                       lst[[j]])
    r = r + 1
  }
}

simMatrix <- matrix(sim,nrow = 5,ncol = 5)
rownames(simMatrix) <- c("Network16","Network17","Network18","Network19","Network20")
colnames(simMatrix) <- c("Network16","Network17","Network18","Network19","Network20")
col<- colorRampPalette(c("red", "grey70", "blue"))(20)
corrplot(simMatrix, type="upper", order="hclust", col=col)

################ cOMMUNITIES

##########################################################################
#                  Communities in Network 2016                           #
##########################################################################
#Getting Top 5 large communities
membership_list <- sort(table(V(g_2016)$k),decreasing = T)
top_com <- as.integer(names(membership_list))[1:5]


g <- delete_vertices(g_2016, V(g_2016)[!(V(g_2016)$k %in% top_com)])
## Making Nodes and Edges dataframe
nodes_df <- data.frame(id = V(g)$name, title = V(g)$name, group = V(g)$k)
edges_df <- get.data.frame(g, what="edges")[1:2]
## Set attributes
nodes_df$title  <- nodes_df$id # Text on click
nodes_df$label  <- nodes_df$id # Node label
nodes_df$font.size <- 45
nodes_df$size <- 25
net2016 = visNetwork(nodes_df, edges_df, main = "Communities in Network 2016") %>%
  visIgraphLayout(layout = "layout_components")
net2016


##########################################################################
#                  Communities in Network 2017                           #
##########################################################################

#getting top 5 large communities
membership_list <- sort(table(V(g_2017)$k),decreasing = T)
top_com <- as.integer(names(membership_list))[1:5]

g <- delete_vertices(g_2017, V(g_2017)[!(V(g_2017)$k %in% top_com)])
## Making Nodes and Edges dataframe
nodes_df <- data.frame(id = V(g)$name, title = V(g)$name, group = V(g)$k)
edges_df <- get.data.frame(g, what="edges")[1:2]
## Set attributes
nodes_df$title  <- nodes_df$id # Text on click
nodes_df$label  <- nodes_df$id # Node label
nodes_df$font.size <- 45
nodes_df$size <- 25
net2017 = visNetwork(nodes_df, edges_df, main = "Communities in Network 2017") %>%
  visIgraphLayout(layout = "layout_components")
net2017

##########################################################################
#                  Communities in Network 2018                           #
##########################################################################

#getting top 5 large communities
membership_list <- sort(table(V(g_2018)$k),decreasing = T)
top_com <- as.integer(names(membership_list))[1:5]

g <- delete_vertices(g_2018, V(g_2018)[!(V(g_2018)$k %in% top_com)])
## Making Nodes and Edges dataframe
nodes_df <- data.frame(id = V(g)$name, title = V(g)$name, group = V(g)$k)
edges_df <- get.data.frame(g, what="edges")[1:2]
## Set attributes
nodes_df$title  <- nodes_df$id # Text on click
nodes_df$label  <- nodes_df$id # Node label
nodes_df$font.size <- 45
nodes_df$size <- 25
net2018 = visNetwork(nodes_df, edges_df, main = "Communities in Network 2018") %>%
  visIgraphLayout(layout = "layout_components")
net2018

##########################################################################
#                  Communities in Network 2019                           #
##########################################################################

#getting top 5 large communities
membership_list <- sort(table(V(g_2019)$k),decreasing = T)
top_com <- as.integer(names(membership_list))[1:5]

g <- delete_vertices(g_2019, V(g_2019)[!(V(g_2019)$k %in% top_com)])
## Making Nodes and Edges dataframe
nodes_df <- data.frame(id = V(g)$name, title = V(g)$name, group = V(g)$k)
edges_df <- get.data.frame(g, what="edges")[1:2]
## Set attributes
nodes_df$title  <- nodes_df$id # Text on click
nodes_df$label  <- nodes_df$id # Node label
nodes_df$font.size <- 45
nodes_df$size <- 25
net2019 = visNetwork(nodes_df, edges_df, main = "Communities in Network 2019") %>%
  visIgraphLayout(layout = "layout_components")
net2019

##########################################################################
#                  Communities in Network 2020                           #
##########################################################################


#getting top 5 large communities
membership_list <- sort(table(V(g_2020)$k),decreasing = T)
top_com <- as.integer(names(membership_list))[1:5]

g <- delete_vertices(g_2020, V(g_2020)[!(V(g_2020)$k %in% top_com)])
## Making Nodes and Edges dataframe
nodes_df <- data.frame(id = V(g)$name, title = V(g)$name, group = V(g)$k)
edges_df <- get.data.frame(g, what="edges")[1:2]
## Set attributes
nodes_df$title  <- nodes_df$id # Text on click
nodes_df$label  <- nodes_df$id # Node label
nodes_df$font.size <- 45
nodes_df$size <- 25
net2020 = visNetwork(nodes_df, edges_df, main = "Communities in Network 2020") %>%
  visIgraphLayout(layout = "layout_components")
net2020



############################THE END#####################################


