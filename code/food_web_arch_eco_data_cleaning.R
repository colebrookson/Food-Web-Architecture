library(tidyverse)
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

#
