library(binford)
library(magrittr)
library(modelr)
library(tidyverse)
library(cowplot)
library(broom)

#### load data ####
#main <- binford::LRB
main <- binford::LRBfact
key <- binford::LRBkey

#key[grep("runoff", key$variable), ]$description

#### data selection ####
main_sel <- main[, c(
  "area", "hunting", "lbio5", "lcoklm", 
  "lrunoff", "watrgrc", "medstab", "perwltg",
  "rlow", "rungrc", "sdtemp"
  )] 

#### create model #####
area_relation <- area ~ hunting + lbio5 + lcoklm + 
                        lrunoff + watrgrc + medstab + perwltg +
                        rlow + rungrc + sdtemp

mod <- main_sel %>% default_model(relation = area_relation)

predictions <- mod %>% unnest(predictions)
resids <- mod %>% unnest(resids)
glance <- mod %>% unnest(glance)

predictions %>% ggplot(aes(hunting, area)) +
  geom_point() +
  geom_line(aes(x = hunting, y = pred), size = 1, colour = "red")

predictions %>% ggplot(aes(watrgrc, area)) +
  geom_point() +
  geom_line(aes(x = watrgrc, y = pred), size = 1, colour = "red")

predictions %>% ggplot(aes(lcoklm, area)) +
  geom_point() +
  geom_line(aes(x = lcoklm, y = pred), size = 1, colour = "red")

predictions %>% ggplot(aes(rlow, area)) +
  geom_point() +
  geom_line(aes(x = rlow, y = pred), size = 1, colour = "red")

#### Model analysis ####

mod$model[[1]] -> fit

#
fit %>% car::avPlots()

#
MASS::stepAIC(fit, direction="both")

#
leaps <- leaps::regsubsets(area_relation, data = main_sel, nbest=10)
plot(leaps, scale="r2")

#
car::subsets(leaps, statistic="rsq")

#
relaimpo::calc.relimp(
    fit, type=c("lmg","last","first","pratt"),
    rela=TRUE
  )

boot <- relaimpo::boot.relimp(
    fit, b = 1000, type = c("lmg","last", "first", "pratt"), rank = TRUE, 
    diff = TRUE, rela = TRUE
  )
relaimpo::booteval.relimp(boot) # print result
plot(relaimpo::booteval.relimp(boot,sort=TRUE)) # plot result

#
plot(fit)

#
area_relation2 <- area ~ hunting + lbio5 + 
  lrunoff + watrgrc + perwltg +
  rungrc + sdtemp

fit2 <- main_sel %>% default_model(relation = area_relation2) %$% model

anova(fit, fit2)

#
library(GGally)
ggpairs(main_sel)
