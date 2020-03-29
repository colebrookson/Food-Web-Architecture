##### TITLE: Food Web Architecture Across Ecosystems
##### CREATOR: Cole B. Brookson
##### INITIALIZATION DATE: 2020-03-17

library(tidyverse)
library(glmmTMB)
library(ggeffects)
library(DHARMa)
library(MuMIn)
library(car)
library(MASS)
library(gamlss)

`%notin%` = negate(`%in%`)
dir = 'C:/Users/coleb/Documents/GitHub/food-web-architecture-across-ecosystems/data'
setwd(dir)

source('food_web_arch_eco_data_cleaning.R') #pull data cleaning and neccessary dataframes

##### Fit regression model to look at the impact of ecosystem on connectance

#probability distribution find
#web_metrics$connectance = round(web_metrics$connectance, digits = 4)
hist(web_metrics$connectance)
qqp(web_metrics$connectance, 'norm') #test normal distribution
qqp(web_metrics$connectance, 'lnorm') #test lognormal distribution

#do nb and gamma using parameter estimates ---- note optim() won't fit the nb version, but gamma fits fine
is.finite(web_metrics$connectance)
gamma_test = fitdistr(web_metrics$connectance, densfun = 'gamma')
qqp(web_metrics$connectance, 'gamma', shape = gamma_test$estimate[[1]], rate = gamma_test$estimate[[2]])

#let's try fitting with a lognormal -- we can use penalized quasiliklihood
conn_pql = glm(connectance ~ ecosystem + mean_dimension + mean_body_size,
                   family = gaussian(link = 'log'),
                   data = web_metrics,
               na.action = 'na.fail')
summary(conn_pql)
plot(conn_pql) #doesn't fit great

#find best model
conn_pql_dredge = MuMIn::dredge(conn_pql)

#okay so top four are the best - model average between them
conn_pql_1 = glm(connectance ~ ecosystem + mean_dimension + mean_body_size,
               family = gaussian(link = 'log'),
               data = web_metrics,
               na.action = 'na.fail')
conn_pql_2 = glm(connectance ~ ecosystem + mean_body_size,
                 family = gaussian(link = 'log'),
                 data = web_metrics,
                 na.action = 'na.fail')
conn_pql_3 = glm(connectance ~ ecosystem + mean_dimension,
                 family = gaussian(link = 'log'),
                 data = web_metrics,
                 na.action = 'na.fail')
conn_pql_4 = glm(connectance ~ ecosystem,
                 family = gaussian(link = 'log'),
                 data = web_metrics,
                 na.action = 'na.fail')







#try fitting as a gam instead with gamlss
conn_gam = gamlss(connectance ~ cs(ecosystem) + cs(mean_dimension) + cs(mean_body_size),
                   family = LNO(),
                  data = web_metrics)
summary(conn_gam)

### Rigby and Stasinopoulos algorithm 

























#get the necessary model selection things:
lepmod.crossedp <- glmmTMB(all.leps ~ spp * site.region + spp * year + 
                             site.region * year + (1 | collection),
                           ziformula = ~1,
                           data = mainlice, family=poisson)
lepmod.crossednb <- glmmTMB(all.leps ~ spp * site.region + spp * year + 
                              site.region * year + (1 | collection),
                            ziformula = ~1,
                            data = mainlice, family=nbinom2)
AICtab(lepmod.crossedp, lepmod.crossednb)
calmod.crossednb <- glmmTMB(all.cal ~ spp * site.region + spp * year + 
                              site.region * year + (1 | collection), 
                            data = mainlice, family=nbinom2)
calmod.crossedp <- glmmTMB(all.cal ~ spp * site.region + spp * year + 
                             site.region * year + (1 | collection), 
                           data = mainlice, family=poisson)
AICtab(calmod.crossedp, calmod.crossednb)

#check models
lep_p_sr <- simulateResiduals(lepmod.crossedp)
cal_nb_sr <- simulateResiduals(calmod.crossednb)
plot(lep_p_sr)
plot(cal_nb_sr)