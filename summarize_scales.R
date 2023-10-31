summarize_scale <- function(data, items, funs, scale = NULL){ 
  if (length(items) == 1) { 
    data_subset <- select(data, 
                          matches(paste0("^", items, "\\ d+$"))) 
    scale <- items } 
  else { data_subset <- select(data, all_of(items))}
  
  if (all(funs == "all")) { 
    funs <- list( 
      mean = mean, 
      mdn = median, 
      maxmode = function(x) { 
        tab <- tabulate(match(x, 
                              unique(x))) 
        max(unique(x)[tab == max(tab)]) 
  }, 
  minmode = function(x) { tab <- tabulate(match(x, unique(x)))
  min(unique(x)[tab == max(tab)]) 
  }, 
  nmax = function(x) sum(x == 
                           max(data_subset, na.rm = TRUE)), 
  nmin = function(x) sum(x == 
                           min(data_subset, na.rm = TRUE)), 
  sd = sd, 
  iqr = function(x) IQR(x, na.rm = TRUE), 
  range = function(x) diff(range(x)), 
  skew = psych::skew, 
  kurtosis = psych::kurtosi, 
  bimodality = mousetrap::bimodality_coefficient, 
  outlier = function(x) max(abs(scale (x))) 
    ) 
  }
  
  out <- apply(data_subset, 1, function(x) 
  {data.frame(lapply(funs, 
                       function(f) f(x)))}) %>% 
    bind_rows() %>%
    mutate(across(where(function(k) 
      any(is.na(k))), replace_na, replace = 0)) 
  names(out) <- paste0(scale, "_", names(funs)) 
  return(out) 
}
