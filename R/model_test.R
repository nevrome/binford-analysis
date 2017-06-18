library(binford)
library(dplyr)
library(magrittr)
library(ggplot2)
library(modelr)

main <- binford::LRB
key <- binford::LRBkey

key[grep("dismov", key$variable), ]$description
 
main_sel <- main[, c("subpop", "density", "dismov", "hunting")] %>%
  dplyr::filter(!is.na(dismov))

main_sel <- main_sel %>% dplyr::mutate(
  ldensity = log10(density)
)

main_sel %>% ggplot(aes(x = ldensity, y = dismov)) +
  geom_hex(bins = 30)

mod_pop <- main_sel %>% lm(dismov ~ ldensity, data = .)

grid <- main_sel %>%
  modelr::data_grid(density = modelr::seq_range(density, 15)) %>%
  dplyr::mutate(ldensity = log10(density)) %>%
  modelr::add_predictions(mod_pop, "dismov")

main_sel %>% ggplot(aes(x = density, y = dismov)) +
  geom_hex(bins = 30) +
  geom_line(data = grid, color = "red", size = 1)

main_sel <- main_sel %>% 
  modelr::add_residuals(mod_pop, "lresid")

main_sel %>% ggplot(aes(ldensity, lresid)) +
  geom_hex(bins = 30)
