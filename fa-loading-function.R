fa_table <- function(x, cut) {
  loadings <- fa.sort(x)$loadings %>% round(3)
  loadings[loadings < cut] <- ""
  loadings %>%
    unclass() %>%
    as.data.frame() %>%
    rownames_to_column("item") %>%
    mutate(across(where(is.numeric), round, 3))
}
