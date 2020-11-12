implied_measurement = function(model, latent=NULL) {
  #browser()
  # get names
  obs_names = lavNames(model)
  latent_names = lavNames(model, type="lv")
  #if (!is.null(latent)) latent_names = latent_names[latent_names %in% latent]
  
  # load dataset
  lav_data = get_all_data(model)

  # estimate matrix of slopes/intercepts (between latent and observed)
  slopes_observed = latent_observed_implied(model) %>% data.frame
  residual_variance = matrix((1-get_observed_rsq(model, obs_names)), nrow=length(obs_names), ncol=length(latent_names), byrow=F)
  slopes_observed = slopes_observed*residual_variance # correction for reliability
  names(slopes_observed) = paste0("slope_", latent_names)
  row.names(slopes_observed) = obs_names
  intercepts_observed = batch_intercepts(slopes_observed, lav_data, latent_names, obs_names)
  names(intercepts_observed) = paste0("intercept_", latent_names)
  
  # transmute to standardized form (for latents), then gather
  obs_standardized = lav_data %>% 
    transmute_at(obs_names, scale) 
  lav_data_std = cbind(obs_standardized, lav_data[,latent_names]) %>% 
    pivot_longer(cols=obs_names, names_to="Variable", values_to="Observed") %>% 
    data.frame %>% 
    set_names(c(latent_names, "Variable", "Observed"))

  # merge the intercept data with the actual data
  slopes_and_intercepts = cbind(slopes_observed, intercepts_observed, Variable=row.names(slopes_observed))

  flex_data = full_join(lav_data_std, slopes_and_intercepts, by="Variable")
  
  ggplot(flex_data, 
         aes(x = Observed, y = latent_x, group = 1)) +         
    geom_point() + 
    facet_wrap(~ Variable) +
    geom_abline(aes(intercept=intercept_latent_x, slope=slope_latent_x, group=1), colour="blue", lwd=2) +
    geom_smooth() + 
    theme_bw() 
    
  
}

batch_intercepts = function(slopes_allvars, lav_data, latent_names, obs_names) {
  
  obs_means = obs_names %>% map_dbl(function(x) mean(lav_data[,x])) %>% 
    matrix(nrow=length(obs_names), ncol=length(latent_names), byrow=F)
  lat_means = latent_names %>% map_dbl(function(x) mean(lav_data[,x])) %>% 
    matrix(nrow=length(obs_names), ncol=length(latent_names), byrow=T)
  intercepts = lat_means - slopes_allvars*obs_means
  return(intercepts)
}

get_observed_rsq = function(model, obs_names) {
  rsq = lavInspect(fit2, "r2")

  # if there's an exogenous variable, set its rsq to one
  exogen_observed = which(!(obs_names %in% names(rsq)))
  rsq[obs_names[exogen_observed]] = 1
  return(rsq[obs_names])
}


loop_flexplot = function(y, data, cov_allvars, slope, intercept) {
  f = make.formula(y, "Observed | Variable")
  p = flexplot(f, data=data) + abline()
}

#model=fit
latent_observed_implied = function(model) {
  
  latent = lavNames(model, type="lv")
  observed = lavNames(model, type="ov")
  
  cor_matrix = lavInspect(model, "cor.all") %>% data.frame
  inter_correlations = cor_matrix[observed, latent]
  return(inter_correlations)
}

