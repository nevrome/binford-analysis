library(binford)
library(magrittr)
library(modelr)
library(tidyverse)
library(cowplot)
library(broom)

main <- binford::LRB
key <- binford::LRBkey

#key[grep("dismov", key$variable), ]$description
 
main_sel <- main[, c("subpop", "density", "dismov", "hunting")] %>%
  dplyr::filter(!is.na(dismov))

main_sel <- main_sel %>% dplyr::mutate(
  ldensity = log10(density)
)

main_sel %>% ggplot(aes(x = ldensity, y = dismov)) +
  geom_hex(bins = 30)

mod_pop <- main_sel %>% lm(dismov ~ ldensity, data = .)

grid <- main_sel %>%
  data_grid(density = seq_range(density, 15)) %>%
  mutate(ldensity = log10(density)) %>%
  add_predictions(mod_pop, "dismov")

main_sel %>% ggplot(aes(x = density, y = dismov)) +
  geom_hex(bins = 30) +
  geom_line(data = grid, color = "red", size = 1)

main_sel <- main_sel %>% 
  modelr::add_residuals(mod_pop, "lresid")

main_sel %>% ggplot(aes(ldensity, lresid)) +
  geom_hex(bins = 30)

####

main_sel <- main[, c("subsp.1", "nagp", "density")] %>%
  mutate(lnagp = log10(nagp), ldensity = log10(density)) %>%
  group_by(subsp.1)

main_sel %>% ggplot(aes(x = lnagp, y = ldensity)) +
  geom_hex(bins = 20) +
  facet_wrap(~subsp.1)


relation <- ldensity ~ lnagp

by_subsp <- default_model(main_sel, relation)
  
predictions <- by_subsp %>% unnest(predictions)
resids <- by_subsp %>% unnest(resids)
glance <- by_subsp %>% unnest(glance)

predictions_plot <- predictions %>% ggplot(aes(lnagp, ldensity)) +
  geom_point() +
  geom_line(aes(x = lnagp, y = pred), size = 1, colour = "red") +
  facet_wrap(~subsp.1)

resids_plot <- resids %>% ggplot(aes(lnagp, resid)) +
  geom_point() +
  facet_wrap(~subsp.1) +
  geom_ref_line(h = 0, size = 1, colour = "black")

plot_grid(predictions_plot, resids_plot, labels = c("model", "residuals"),  ncol = 1, nrow = 2)
