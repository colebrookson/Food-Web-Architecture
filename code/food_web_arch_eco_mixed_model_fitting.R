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

#get the predictions for each one
conn_pql_1_pred = ggpredict(conn_pql_1, terms = 'ecosystem')
conn_pql_2_pred = ggpredict(conn_pql_2, terms = 'ecosystem')
conn_pql_3_pred = ggpredict(conn_pql_3, terms = 'ecosystem')
conn_pql_4_pred = ggpredict(conn_pql_4, terms = 'ecosystem')

#put them all together, weight them and get the weighted-average
conn_pql_all_pred = data.frame(cbind(conn_pql_1_pred$predicted, conn_pql_2_pred$predicted, 
                                     conn_pql_3_pred$predicted, conn_pql_4_pred$predicted,
                                     conn_pql_1_pred$conf.high, conn_pql_2_pred$conf.high, 
                                     conn_pql_3_pred$conf.high, conn_pql_4_pred$conf.high,
                                     conn_pql_1_pred$conf.low, conn_pql_2_pred$conf.low, 
                                     conn_pql_3_pred$conf.low, conn_pql_4_pred$conf.low)) %>% 
  rename(conn_1 = X1, conn_2 = X2, conn_3 = X3, conn_4 = X4,
         conn_1_h = X5, conn_2_h = X6, conn_3_h = X7, conn_4_h = X8,
         conn_1_l = X9, conn_2_l = X10, conn_3_l = X11, conn_4_l = X12)

conn_pql_all_pred = conn_pql_all_pred %>% 
  mutate(w1 = rep(conn_pql_dredge$weight[1], nrow(conn_pql_all_pred)), 
         w2 = rep(conn_pql_dredge$weight[2], nrow(conn_pql_all_pred)),
         w3 = rep(conn_pql_dredge$weight[3], nrow(conn_pql_all_pred)),
         w4 = rep(conn_pql_dredge$weight[4], nrow(conn_pql_all_pred)))

conn_pql_all_pred = conn_pql_all_pred %>% 
  mutate(conn_1w = conn_1*w1, conn_2w = conn_2*w2, conn_3w = conn_3*w3, conn_4w = conn_4*w4,
         conn_1_hw = conn_1_h*w1, conn_2_hw = conn_2_h*w2, conn_3_hw = conn_3_h*w3, conn_4_hw = conn_4_h*w4,
         conn_1_lw = conn_1_l*w1, conn_2_lw = conn_2_l*w2, conn_3_lw = conn_3_l*w3, conn_4_lw = conn_4_l*w4) %>% 
  mutate(avg = conn_1w + conn_2w + conn_3w + conn_4w,
         avg_h = conn_1_hw + conn_2_hw + conn_3_hw + conn_4_hw,
         avg_l = conn_1_lw + conn_2_lw + conn_3_lw + conn_4_lw)

conn_pql_avg_pred = conn_pql_all_pred %>% 
  dplyr::select(avg, avg_h, avg_l) %>% 
  mutate(ecosystem = conn_pql_1_pred$x)




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