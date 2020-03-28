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
                    guess_max = 222151)

#look at the data
head(all_webs)
sapply(all_webs, class)
unique(all_webs$con.common) #to check if parser worked properly

#make all the species names one string
all_webs$con.taxonomy = gsub('\\s+', '_', all_webs$con.taxonomy)
all_webs$res.taxonomy = gsub('\\s+', '_', all_webs$res.taxonomy)

#how many unique webs are there?
sort(unique(all_webs$foodweb.name)) #290

##### make an edge and node lists for all web
get_edges_and_nodes = function(all_webs){
  
  list_of_webs = list() #Create a list in which you intend to save your df's.
  
  j = 1 #start positional iteration
  
  web_names = sort(unique(all_webs$foodweb.name)) #get list of web names
  web_names = gsub('\\s+', '_', web_names) #remove spaces
  
  for(i in unique(web_names)){ #Loop through the numbers of ID's instead of the ID's
    
    #pick a web
    web = all_webs %>% 
      filter(foodweb.name == i)
    
    web_list = list()

    #keep variables
    web = web %>% 
      select(autoID, interaction.type, con.taxonomy, con.taxonomy.level, con.lifestage, 
             con.mass.mean.g., res.taxonomy, res.taxonomy.level, res.lifestage,
             res.mass.mean.g., ecosystem.type)
    
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
             ecosystems = ecosystem.type)
    web_con_node = web[,c(3:6,11)] %>% 
      distinct() %>% 
      rename(species = con.taxonomy,
             taxonomy_level = con.taxonomy.level,
             lifestage = con.lifestage,
             mean_mass = con.mass.mean.g.,
             ecosystems = ecosystem.type)
    web_nodes = rbind(web_res_node, web_con_node)
    
    #make edge list
    web_edges = web %>% 
      select(con.taxonomy, res.taxonomy) %>% 
      rename(consumer = con.taxonomy, 
             resource = res.taxonomy) %>% 
      distinct()
    
    #save dataframes into the list
    
    sub_list_names = c(paste0(i, '_edges'), paste0(i, '_nodes'))
    
    web_list[sub_list_names[1]] = assign(paste0(i, '_edges'), web_edges)
    web_list[sub_list_names[2]] = assign(paste0(i, '_nodes'), web_nodes)
    
    list_of_webs[[i]] <- assign(paste0(i,'_web_list')) 
    list_names[j] = paste0(i, '')
    #iterate
    j = j + 1
  }
  
  return(list_of_webs) #Return the list of dataframes.
}

webs_nodes_and_edges = get_edges_and_nodes(all_webs = all_webs)



test = webs_nodes_and_edges[[1]][[1]]











###### SEND EMAIL RE: JOURNAL GROUP



####################### CURRENTLY: trying to get an SxS matrix from the data








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
