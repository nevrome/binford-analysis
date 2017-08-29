library(binford)
library(purrr)
library(dplyr)
library(magrittr)
library(reshape2)

main <- binford::LRB
key <- binford::LRBkey

unique(key$class)
unique(key$Fclass)
unique(key$type)


# automatic type classification
# level_info <- list(key$Fclass, key$type) %>% purrr::pmap_chr(
#   function(x, y) {
#     if (x == "numeric" & y == "categorical") { return("nominal") }
#     if (x == "numeric" & y == "ordinal"    ) { return("ratio") }
#     if (x == "integer" & y == "categorical") { return("nominal") }
#     if (x == "integer" & y == "ordinal"    ) { return("nominal") } # case not existend
#     if (x == "factor"  & y == "categorical") { return("nominal") }
#     if (x == "factor"  & y == "ordinal"    ) { return("ordinal") }
#   }
# )

# ratio_vars_index <- which(level_info == "ratio")
# 
# ratio_vars <- main[, ratio_vars_index]

ratio_vars_names <- key %>% dplyr::filter(
  class == "numeric",
  Fclass == "numeric",
  type == "ordinal"
) %$% X

ratio_vars <- main[, colnames(main) %in% ratio_vars_names]  

cor_matrix <- cor(ratio_vars, use = "pairwise.complete.obs", method = "pearson")

cor_matrix[lower.tri(cor_matrix, diag = TRUE)] <- NA

cor_list <- melt(cor_matrix) %>%
  dplyr::filter(
    value %>% is.na %>% `!`
  )

cor_sel <- cor_list %>% dplyr::filter(
  Var1 != Var2,
  value != 1,
  abs(value) >= 0.9
) %>% dplyr::arrange(
  value %>% abs %>% desc
)

cor_sel2 <- cor_sel %>% dplyr::left_join(
  y = key[c("X", "description")], by = c("Var1" = "X")
) %>% dplyr::left_join(
  y = key[c("X", "description")], by = c("Var2" = "X")
)
