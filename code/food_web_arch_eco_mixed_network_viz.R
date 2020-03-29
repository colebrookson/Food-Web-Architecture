##### PROJECT TITLE: Food Web Architecture Across Ecosystems
##### CREATOR: Cole B. Brookson
##### INITIALIZATION DATE: 2020-03-29

dir = 'C:/Users/coleb/Documents/GitHub/food-web-architecture-across-ecosystems/data'
setwd(dir)

source('food_web_arch_eco_data_cleaning.R') #pull data cleaning and neccessary dataframes
source('food_web_arch_eco_model_fitting.R') #pull data cleaning and neccessary dataframes

library(igraph)

#make igraph data
edges = webs_nodes_and_edges[['SEW48']][[1]]
nodes = webs_nodes_and_edges[['SEW48']][[2]] %>% 
  distinct()

demo_net = graph_from_data_frame(d = edges, 
                                 vertices = nodes, 
                                 directed = FALSE)
n_distinct(nodes$species)
