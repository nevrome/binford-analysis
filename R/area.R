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

mod$model[[1]] %>% car::avPlots()

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

