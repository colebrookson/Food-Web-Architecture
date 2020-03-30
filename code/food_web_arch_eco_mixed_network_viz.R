##### PROJECT TITLE: Food Web Architecture Across Ecosystems
##### CREATOR: Cole B. Brookson
##### INITIALIZATION DATE: 2020-03-29

dir = 'C:/Users/coleb/Documents/GitHub/food-web-architecture-across-ecosystems/data'
setwd(dir)

source('C:/Users/coleb/Documents/GitHub/food-web-architecture-across-ecosystems/code/food_web_arch_eco_data_cleaning.R') #pull data cleaning and neccessary dataframes
source('C:/Users/coleb/Documents/GitHub/food-web-architecture-across-ecosystems/code/food_web_arch_eco_mixed_model_fitting.R') #pull data cleaning and neccessary dataframes

library(igraph)
library(scales)

#lake example

#make igraph data
edges = webs_nodes_and_edges[['Alford_lake']][[1]]
nodes = webs_nodes_and_edges[['Alford_lake']][[2]] %>% 
  distinct()

demo_net = graph_from_data_frame(d = edges, 
                                 vertices = nodes, 
                                 directed = FALSE)
demo_net

#make some plots
graph_attr(demo_net, 'layout') = layout_with_lgl


#size = V(demo_net)$mean_mass
#size[25] = 7.000e-02; size[13] = 5.20e-02; size[6] = 1.8200e-02
#V(demo_net)$size = size*500
circle_option = layout_in_circle(demo_net)
force_directed = layout_with_fr(demo_net)
png('alford_lake_random.png', width = 800, height = 800)
plot(demo_net, vertex.label = NA, layout = layout_randomly, 
     vertex.color = '#00a572', 
     vertex.shape = 'circle',
     vertex.size = 10, 
     main = 'Alford Lake', 
     cex.main = 10, 
     cex.lab = 10)
dev.off()
png('alford_lake_circle.png',
    width = 800, height = 800)
plot(demo_net, vertex.label = NA, layout = circle_option, 
     vertex.color = '#00a572', 
     vertex.shape = 'circle',
     vertex.size = 10, 
     main = 'Alford Lake', 
     cex.main = 5,
     cex.lab = 10)
dev.off()

#make igraph data
edges = webs_nodes_and_edges[['Carpinteria']][[1]]
nodes = webs_nodes_and_edges[['Carpinteria']][[2]] %>% 
  distinct()

demo_net = graph_from_data_frame(d = edges, 
                                 vertices = nodes, 
                                 directed = FALSE)
demo_net

#make some plots
graph_attr(demo_net, 'layout') = layout_with_lgl


#size = V(demo_net)$mean_mass
#size[25] = 7.000e-02; size[13] = 5.20e-02; size[6] = 1.8200e-02
#V(demo_net)$size = size*500
circle_option = layout_in_circle(demo_net)
force_directed = layout_with_fr(demo_net)
png('carpinteria_random.png', width = 800, height = 800)
plot(demo_net, vertex.label = NA, layout = layout_randomly, 
     vertex.color = '#0B6623', 
     vertex.shape = 'circle',
     vertex.size = 8, 
     main = 'Carpinteria')
dev.off()
png('carpinteria_circle.png', width = 800, height = 800)
plot(demo_net, vertex.label = NA, layout = circle_option, 
     vertex.color = '#0B6623', 
     vertex.shape = 'circle',
     vertex.size = 4, 
     edge.width = 0.1,
     main = 'Carpinteria')
dev.off()

#make igraph data
edges = webs_nodes_and_edges[['Stony_Stream']][[1]]
nodes = webs_nodes_and_edges[['Stony_Stream']][[2]] %>% 
  distinct()

demo_net = graph_from_data_frame(d = edges, 
                                 vertices = nodes, 
                                 directed = FALSE)
demo_net

#make some plots
graph_attr(demo_net, 'layout') = layout_with_lgl


#size = V(demo_net)$mean_mass
#size[25] = 7.000e-02; size[13] = 5.20e-02; size[6] = 1.8200e-02
#V(demo_net)$size = size*500
circle_option = layout_in_circle(demo_net)
force_directed = layout_with_fr(demo_net)
png('stony_stream_random.png', width = 800, height = 800)
plot(demo_net, vertex.label = NA, layout = layout_randomly, 
     vertex.color = '#d0efff', 
     vertex.shape = 'circle',
     vertex.size = 8, 
     main = 'Stony Stream')
dev.off()
png('stony_stream_circle.png', width = 800, height = 800)
plot(demo_net, vertex.label = NA, layout = circle_option, 
     vertex.color = '#d0efff', 
     vertex.shape = 'circle',
     vertex.size = 10, 
     edge.width = 0.1,
     main = 'Stony Stream')
dev.off()

edges = webs_nodes_and_edges[['Dutch_Microfauna_food_web_PlotA']][[1]]
nodes = webs_nodes_and_edges[['Dutch_Microfauna_food_web_PlotA']][[2]] %>% 
  distinct()

demo_net = graph_from_data_frame(d = edges, 
                                 vertices = nodes, 
                                 directed = FALSE)
demo_net

#make some plots
graph_attr(demo_net, 'layout') = layout_with_lgl


#size = V(demo_net)$mean_mass
#size[25] = 7.000e-02; size[13] = 5.20e-02; size[6] = 1.8200e-02
#V(demo_net)$size = size*500
circle_option = layout_in_circle(demo_net)
force_directed = layout_with_fr(demo_net)
png('dutch_random.png', width = 800, height = 800)
plot(demo_net, vertex.label = NA, layout = layout_randomly, 
     vertex.color = '#4B5320', 
     vertex.shape = 'circle',
     vertex.size = 8, 
     main = 'Dutch Plot A')
dev.off()
png('dutch_circle.png', width = 800, height = 800)
plot(demo_net, vertex.label = NA, layout = circle_option, 
     vertex.color = '#4B5320', 
     vertex.shape = 'circle',
     vertex.size = 6, 
     edge.width = 0.1,
     main = 'Dutch Plot A')
dev.off()

edges = webs_nodes_and_edges[['Chesapeake_Bay']][[1]]
nodes = webs_nodes_and_edges[['Chesapeake_Bay']][[2]] %>% 
  distinct()

demo_net = graph_from_data_frame(d = edges, 
                                 vertices = nodes, 
                                 directed = FALSE)
demo_net

#make some plots
graph_attr(demo_net, 'layout') = layout_with_lgl


#size = V(demo_net)$mean_mass
#size[25] = 7.000e-02; size[13] = 5.20e-02; size[6] = 1.8200e-02
#V(demo_net)$size = size*500
circle_option = layout_in_circle(demo_net)
force_directed = layout_with_fr(demo_net)
png('chesapeake_random.png', width = 1000, height = 1000)
plot(demo_net, vertex.label = NA, layout = layout_randomly, 
     vertex.color = '#03254c', 
     vertex.shape = 'circle',
     vertex.size = 6, 
     main = 'Chesapeake Bay')
dev.off()
png('chesapeake_circle.png', width = 1000, height = 1000)
plot(demo_net, vertex.label = NA, layout = circle_option, 
     vertex.color = '#03254c', 
     vertex.shape = 'circle',
     vertex.size = 2, 
     edge.width = 0.1,
     main = 'Chesapeake Bay')
dev.off()

# #playing with layouts
# layouts = grep("^layout_", ls("package:igraph"), value=TRUE)[-1]
# # Remove layouts that do not apply to our graph.
# layouts = layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]
# 
# par(mfrow=c(3,3), mar=c(1,1,1,1))
# for (layout in layouts) {
#   print(layout)
#   l = do.call(layout, list(demo_net))
#   plot(demo_net, layout = l, main = layout, vertex.label = NA)
# }
