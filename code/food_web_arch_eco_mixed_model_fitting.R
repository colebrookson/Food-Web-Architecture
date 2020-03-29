##### PROJECT TITLE: Food Web Architecture Across Ecosystems
##### CREATOR: Cole B. Brookson
##### INITIALIZATION DATE: 2020-03-29

`%notin%` = negate(`%in%`)
dir = 'C:/Users/coleb/Documents/GitHub/food-web-architecture-across-ecosystems/data'
setwd(dir)

source('food_web_arch_eco_data_cleaning.R') #pull data cleaning and neccessary dataframes

library(ggeffects)
library(DHARMa)
library(MuMIn)
library(car)
library(MASS)
library(gamlss)

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
  mutate(ecosystem = conn_pql_1_pred$x) %>% 
  mutate(agg_eco = c('terrestrial', 'aquatic', 'terrestrial', 'aquatic', 'aquatic'))

conn_pql_avg_pred$ecosystem = factor(conn_pql_avg_pred$ecosystem, levels = c('terrestrial_belowground', 'streams', 'terrestrial_aboveground', 
                                       'lakes', 'marine'),
         labels = c('Belowground', 'Streams', 'Aboveground', 'Lakes', 'Marine'))

#make some effects plots
theme7 <- function(){
  color.background = 'white'
  color.grid.major = 'black'
  color.axis.text = 'black'
  color.axis.title = 'black'
  color.title = 'black'
  theme_bw(base_size = 9) + 
    theme(panel.background = element_rect(fill=color.background,color = color.background)) +
    theme(plot.background = element_rect(fill = color.background, color = color.background)) +
    theme(panel.border = element_rect(colour = 'black', size = 1.5)) +
    theme(panel.grid.major = element_blank()) + 
    theme(panel.grid.minor = element_blank()) + 
    theme(axis.line.x.bottom = element_line(colour = 'black', size = 1))+
    theme(axis.line.x.top = element_line(colour = 'black', size = 1))+
    theme(axis.line.y.left = element_line(colour = 'black', size = 1))+
    theme(axis.line.y.right = element_line(colour = 'black', size = 1))+
    theme(axis.ticks = element_blank()) +
    theme(plot.title = element_text(color = color.title, size = 19, vjust = 1.25, margin = margin(t = 0, r = 0, b = 20, l = 0))) +
    theme(axis.text.x = element_text(size = 14, color = color.axis.text, angle = 90)) + 
    theme(axis.text.y = element_text(size = 14, color = 'black')) + 
    theme(axis.title.x = element_text(size = 16, color = color.axis.title, vjust = 0, margin = margin(t = 17, r = 0, b = 17, l = 0))) +
    theme(axis.title.y = element_text(size = 16, color = 'black', vjust = 1.5, margin = margin(t = 0, r = 17, b = 0, l = 17))) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(strip.background = element_rect(fill = 'white', colour = 'grey70'),
          strip.placement = 'outside',
          strip.text = element_text(size = 10))+
    #theme(legend.position = c(0.4,0.6)) +
    theme(legend.text = element_text(size = 10, margin = margin(t = 0, r = 0, b = 0, l = 0)),
          legend.title = element_text(size = 12, margin = margin(t = 0, r = 0, b = 0, l = 0)))
          #legend.key.width= unit(4.5, 'line'),
          #legend.key.height = unit(0.7,'line'))
}

leg_title <- 'Ecosystem \nType'
conn_pred_plot = conn_pql_avg_pred %>% 
  ggplot(aes(x = ecosystem, y = avg, colour = ecosystem, shape = agg_eco)) +
  scale_shape_manual(values = c(15,17), labels = c('Aquatic', 'Terrestrial')) +
  geom_errorbar(aes(ymin=avg_l, ymax = avg_h,width = 0), size = 0.78, colour = 'black')+
  geom_point(size = 4.7) +
  scale_color_manual(leg_title,values=c('#4B5320','#d0efff', '#0B6623', '#00a572', '#03254c'), 
                     labels = c('Terrestrial, Belowground', 'Streams', 'Terrestrial, Aboveground',
                                'Lakes', 'Marine'))+
  labs(x = 'Ecosystem Types', y = 'Predicted Connectance') +
  guides(shape = guide_legend(title = 'Aggregated \nEcosystem', override.aes = list(shape = c(0,2)), type = 'a')) +
  theme7()
  #scale_y_continuous(limits = c(0,1.5), breaks = c(0.5,1.0,1.5))+
 # fte_theme1()




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