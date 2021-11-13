return_residual_dataset = function(fitted, max_val = 0.01) {
  obs_names = lavNames(fitted)
  residual_correlations = residuals(fitted, type="cor")$cov 
  
  # string out correlations
  vechs_cors = vechs(residual_correlations)
  pairwise_names = name_vechs(obs_names)
  
  # subset data
  condition = which(abs(vechs_cors)>max_val)
  
  # create dataset
  res_d = data.frame(Residual=(vechs_cors[condition]), Correlation = pairwise_names[condition])
  res_d = res_d[order(abs(res_d$Residual), decreasing = T),]
  
  # convert correlations into ordered factor (so axes are consistent)
  res_d$Correlation = factor(res_d$Correlation, levels=res_d$Correlation, ordered=T)
  
  return(res_d)
}

combine_residual_datasets = function(fitted, fitted2=NULL, max_val=.01) {
  
  if (is.null(fitted2)) return(return_residual_dataset(fitted, max_val))
  
  # get the first dataset, but set maxval to zero
  d_1 = return_residual_dataset(fitted, max_val=0)
  d_2 = return_residual_dataset(fitted2, max_val=0)
  
  # merge the datasets, sort, and gather
  d = merge(d_1, d_2, by="Correlation") %>% 
    filter(abs(Residual.x) > max_val | abs(Residual.y) > max_val ) %>% 
    mutate(average_residual = abs(Residual.x) + abs(Residual.y)) %>% 
    arrange(average_residual) %>% 
    gather(key="Model", value="Residual", Residual.x, Residual.y)
  
  return(d)
}

name_vechs = function(variable_names, collapse=":"){
  combn(variable_names, 
        m=2, 
        FUN = function(x) paste0(x[1], collapse, x[2]))
}