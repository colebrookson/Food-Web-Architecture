##### TITLE: Food Web Architecture Across Ecosystems
##### CREATOR: Cole B. Brookson
##### INITIALIZATION DATE: 2020-03-17

library(tidyverse)
library(fastDummies)
dir = 'C:/Users/coleb/Documents/GitHub/food-web-architecture-across-ecosystems/data'
setwd(dir)

all_webs_spec = spec_csv('283_2_FoodWebDataBase_2018_12_10.csv') #test how read in will work
all_webs_spec

all_webs = read_csv('283_2_FoodWebDataBase_2018_12_10.csv', 
                    guess_max = 222151,
                    na = c('-999', 'NA', 'NaN', 'nan'))

#look at the data
head(all_webs)
sapply(all_webs, class)
unique(all_webs$con.common) #to check if parser worked properly

#make all the species names one string
all_webs$con.taxonomy = gsub('\\s+', '_', all_webs$con.taxonomy)
all_webs$res.taxonomy = gsub('\\s+', '_', all_webs$res.taxonomy)
all_webs$foodweb.name = gsub('\\s+', '_', all_webs$foodweb.name) #remove spaces

#how many unique webs are there?
sort(unique(all_webs$foodweb.name)) #290

##### make a function to get all edge and node lists for all webs

get_edges_and_nodes = function(all_webs){
  
  list_of_webs = list() #Create a list in which you intend to save your df's.
  
  web_names = sort(unique(all_webs$foodweb.name)) #get list of web names
  web_names = gsub('\\s+', '_', web_names) #remove spaces
  
  for(i in unique(web_names)){ #Loop through the numbers of ID's instead of the ID's
    
    #pick a web
    web = all_webs %>% 
      filter(foodweb.name == i)
    
    web_list <- vector(mode = "list", length = 2)

    #keep variables
    web = web %>% 
      select(autoID, interaction.type, con.taxonomy, con.taxonomy.level, con.lifestage, 
             con.mass.mean.g., res.taxonomy, res.taxonomy.level, res.lifestage,
             res.mass.mean.g., ecosystem.type, interaction.dimensionality)
    
    #only keep the consumptive predatory interactions (no herbivores)
    web = web %>% 
      filter(interaction.type == 'predacious')
    
    #make node list
    web_res_node = web[,7:ncol(web)] %>% 
      distinct() %>% 
      rename(species = res.taxonomy,
             taxonomy_level = res.taxonomy.level,
             lifestage = res.lifestage,
             mean_mass = res.mass.mean.g.,
             ecosystems = ecosystem.type,
             dimensions = interaction.dimensionality)
    web_con_node = web[,c(3:6,11,12)] %>% 
      distinct() %>% 
      rename(species = con.taxonomy,
             taxonomy_level = con.taxonomy.level,
             lifestage = con.lifestage,
             mean_mass = con.mass.mean.g.,
             ecosystems = ecosystem.type,
             dimensions = interaction.dimensionality)
    web_nodes = rbind(web_res_node, web_con_node)
    web_nodes$dimensions = as.integer(substr(web_nodes$dimensions,1,1))
    
    #make edge list
    web_edges = web %>% 
      select(con.taxonomy, res.taxonomy) %>% 
      rename(consumer = con.taxonomy, 
             resource = res.taxonomy) %>% 
      distinct()
    
    #save dataframes into the list
    sub_list_names = c(paste0(i, '_edges'), paste0(i, '_nodes')) #make web_nodes and web_edges names
    
    web_list[[1]] = web_edges; web_list[[2]] = web_nodes #put edges and nodes dfs in sublist
    
    list_of_webs[[i]] <- web_list #assign sublist to larger list
    
    names(list_of_webs[[i]]) = sub_list_names #name sublocation 
    
  }

  return(list_of_webs) #Return the list of dataframes.
}

webs_nodes_and_edges = get_edges_and_nodes(all_webs = all_webs)

##### write all dataframes out for easy access in python
# for(i in 1:length(webs_nodes_and_edges)) {
#   for(j in 1:2) {
#     
#     path = paste0(dir, '/', names(webs_nodes_and_edges[[i]][j]), '.csv') #get write path
#     
#     temp = data.frame(webs_nodes_and_edges[[i]][j]) #make temp dataframe for writing
#     
#     colnames(temp) = c('consumer', 'resource') #reassign column names
#     
#     write_csv(temp, path = path) #write file
#   }
# }

##### calculate metrics for all the webs

#### maybe calculate degree distributions for each one and then an aggregated ecosystem version 
#### maybe also calculate network diameter 
#### look at clustering coefficient (global,  cc2 = (3Nt)/(Nc)) where Nt is the number of triangles in graph G and Nc is the number of three-node subgraphs
#### or maybe do modularity instead
#### look at nestedness as well 
#### look at intervality 
#### look at all the values of centrality

#make initial dataframe
web_names = sort(unique(all_webs$foodweb.name))
web_metrics = data.frame(sort(unique(web_names))) 
web_metrics = web_metrics %>% 
  rename(webs = names(web_metrics))

connectance = vector(mode = 'numeric', length = 290) #initialize vector to store connectance values in
ecosystem = vector(mode = 'character', length = 290) #initialize vector to store ecosystem type values in
mean_body_size = vector(mode = 'numeric', length = 290) #initialize vector to store mean body size values in
mean_dimension = vector(mode = 'numeric', length = 290) #initialize vector to store mean dimension in

j = 1
for(i in sort(unique(web_names))) {
  
  df_edges = webs_nodes_and_edges[[i]][[1]]
  df_nodes = webs_nodes_and_edges[[i]][[2]]
  
  ## Connectance
  #taking from Delmas et al. (2017) - C = L/m, where m = S((S+1)/2) (undirected network where species can interact with themselves)
  L = nrow(df_edges)
  S = nrow(df_nodes)
  m = (S*((S+1)/2))
  connectance[j] = L/m
  
  ## Values from web
  ecosystem[j] = df_nodes$ecosystems[1]
  mean_body_size[j] = mean(df_nodes$mean_mass, na.rm = TRUE)
  mean_dimension[j] = mean(df_nodes$dimensions, na.rm = TRUE)
  
  j = j+1
  
}

web_metrics$connectance = connectance
web_metrics$ecosystem = ecosystem
web_metrics$mean_body_size = mean_body_size
web_metrics$mean_dimension = mean_dimension

#note: South_Lake web is apparently empty ---- check why and remove if necessary
webs_nodes_and_edges[['South_Lake']] #these dataframes are empty
south_lake = all_webs %>% 
  filter(foodweb.name == 'South_Lake') 
unique(south_lake$interaction.type) #all herbivorous - so remove this from the data frame

web_metrics = web_metrics %>% 
  filter(web_names != 'South_Lake')

#add aggregated ecosystems
web_metrics$agg_ecosystem = '' 
unique(web_metrics$ecosystem)
for(i in 1:nrow(web_metrics)) {
  web_metrics$agg_ecosystem[i] = ifelse(web_metrics$ecosystem[i] == 'terrestrial aboveground', 'terrestrial', 
                                        ifelse(web_metrics$ecosystem[i] == 'terrestrial belowground', 'terrestrial',
                                               ifelse(web_metrics$ecosystem[i] == 'streams', 'aquatic',
                                                      ifelse(web_metrics$ecosystem[i] == 'marine', 'aquatic',
                                                             ifelse(web_metrics$ecosystem[i] == 'lakes', 'aquatic',NA)))))
}

#check evenness of sampling across the ecosystems
table(web_metrics$ecosystem) #definitely a marine/aquatic bias








###### SEND EMAIL RE: JOURNAL GROUP

################################# UNUSED CODE ############################################

# #turn into matrix
# 
# 
# #keep only the species columns
# ythan_adj = ythan_adj[,c(3,14:ncol(ythan_adj))]
# 
# #summarize interactions across body sizes, take max of rows (because not looking at size bins)
# ythan_adj = ythan_adj %>% 
#   group_by(con.taxonomy) %>% 
#   summarize_all(funs(max))
# 
# #add remaining rows to make a balanced matrix
# rownames(ythan_adj) = ythan_adj$con.taxonomy
# ythan_adj = ythan_adj %>% 
#   select(-con.taxonomy)
# 
# #get rid of colname prefixes
# names(ythan_adj) = substring(names(ythan_adj), 14)
# 
# res_names = as.vector(unique(ythan$res.taxonomy))
# res_names = res_names[!duplicated(res_names,rownames(ythan_adj))]
# union(res_names,rownames(ythan_adj))
# 
# test = dummy_rows(ythan_adj,
#                   select_columns = res_names,
#                   dummy_value = 0)
# 
# 
# 
# #now get rid of prefixes to the names
# ythan_species = sort(unique(ythan_adj$res.taxonomy))
# original_col_names = colnames(ythan)
# new_col_names = c(as.character(original_col_names[1:21]), as.character(trait_names))
# colnames(traits_multivar) = new_col_names
