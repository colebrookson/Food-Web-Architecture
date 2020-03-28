##### TITLE: Food Web Architecture Across Ecosystems
##### CREATOR: Cole B. Brookson
##### INITIALIZATION DATE: 2020-03-17

library(tidyverse)
library(fastDummies)
dir = 'C:/Users/coleb/Documents/GitHub/food-web-architecture-across-ecosystems/data'
setwd(dir)

all_webs_spec = spec_csv('283_2_FoodWebDataBase_2018_12_10.csv')
all_webs_spec

all_webs = read_csv('283_2_FoodWebDataBase_2018_12_10.csv', 
                    guess_max = 222151)

#look at the data
head(all_webs)
sapply(all_webs, class)
unique(all_webs$con.common) #to check if parser worked properly

#make all the species names one string
all_webs$con.taxonomy <- gsub('\\s+', '_', all_webs$con.taxonomy)
all_webs$res.taxonomy <- gsub('\\s+', '_', all_webs$res.taxonomy)

#how many unique webs are there?
sort(unique(all_webs$foodweb.name)) #290

##### make an adjacency matrix for a single web

#pick a web
ythan = all_webs %>% 
  filter(foodweb.name == 'Ythan Estuary')
ythan_edge_info = ythan

#keep variables
ythan = ythan %>% 
  select(autoID, interaction.type, con.taxonomy, con.taxonomy.level, con.lifestage, 
         con.mass.mean.g., res.taxonomy, res.taxonomy.level, res.lifestage, con.common,
         res.common, res.mass.mean.g., ecosystem.type)

#only keep the consumptive predatory interactions (no herbivores)
ythan = ythan %>% 
  filter(interaction.type == 'predacious')

#turn into matrix
ythan_adj = fastDummies::dummy_cols(ythan, select_columns = c('res.taxonomy'),
                                    remove_selected_columns = FALSE)

#keep only the species columns
ythan_adj = ythan_adj[,c(3,14:ncol(ythan_adj))]

#summarize interactions across body sizes, take max of rows (because not looking at size bins)
ythan_adj = ythan_adj %>% 
  group_by(con.taxonomy) %>% 
  summarize_all(funs(max))

#add remaining rows to make a balanced matrix
rownames(ythan_adj) = ythan_adj$con.taxonomy
ythan_adj = ythan_adj %>% 
  select(-con.taxonomy)

#get rid of colname prefixes
names(ythan_adj) = substring(names(ythan_adj), 14)

res_names = as.vector(unique(ythan$res.taxonomy))
res_names = res_names[!duplicated(res_names,rownames(ythan_adj))]
union(res_names,rownames(ythan_adj))

test = dummy_rows(ythan_adj,
                  select_columns = res_names,
                    dummy_value = 0)



#now get rid of prefixes to the names
ythan_species = sort(unique(ythan_adj$res.taxonomy))
original_col_names = colnames(ythan)
new_col_names = c(as.character(original_col_names[1:21]), as.character(trait_names))
colnames(traits_multivar) = new_col_names


###### SEND EMAIL RE: JOURNAL GROUP



####################### CURRENTLY: trying to get an SxS matrix from the data