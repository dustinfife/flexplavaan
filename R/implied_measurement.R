implied_measurement = function(model, latent=NULL) {
  # get long-format, standardized data
  flex_data = prepare_measurement_data(model)

  if (is.null(latent)) latent = get_names(model)[[2]]
  
  plots = latent %>% purrr::map(function(x) latent_flexplot(flex_data, x))
  print(plots)
}



latent_flexplot = function(flex_data, latent) {
  intercept_name = paste0("intercept_", latent)
  slope_name = paste0("slope_", latent)
  ggplot(flex_data, 
         aes_string(x = "Observed", y = latent, group = "1")) +         
    geom_point() + 
    facet_wrap(~ Variable) +
    geom_abline(aes_string(intercept=intercept_name, slope=slope_name, group="1"), colour="blue", lwd=2) +
    geom_smooth() + 
    theme_bw()
}

prepare_measurement_data = function(model) {
  # get names
  names = get_names(model)
  obs_names = names[[1]]; latent_names = names[[2]]
  
  # load dataset
  lav_data = standardize_observed(model)
  
  # estimate matrix of slopes/intercepts (between latent and observed)
  slopes_observed = get_slopes(model, obs_names, latent_names)
  intercepts_observed = get_intercepts(slopes_observed, lav_data, latent_names, obs_names)
  
  # convert from wide to long format
  lav_data_std = lav_data %>% 
    pivot_longer(cols=obs_names, names_to="Variable", values_to="Observed") %>% 
    data.frame %>% 
    purrr::set_names(c(latent_names, "Variable", "Observed"))
  
  # merge the intercept data with the actual data
  slopes_and_intercepts = cbind(slopes_observed, intercepts_observed, Variable=row.names(slopes_observed))
  flex_data = full_join(lav_data_std, slopes_and_intercepts, by="Variable")
  return(flex_data)
}

# this function converts all observed variables to standardized form
standardize_observed = function(model) {
  lav_data = get_all_data(model)
  names = get_names(model)
  obs_names = names[[1]]; latent_names = names[[2]]  
  
  obs_standardized = lav_data %>% 
    transmute_at(obs_names, scale)
  lav_data[,obs_names] = obs_standardized[,obs_names]
  return(lav_data)
}

get_slopes = function(model, obs_names=NULL, latent_names=NULL) {
  
  if (is.null(obs_names) | is.null(latent_names)) {
    # get names
    names = get_names(model)
    obs_names = names[[1]]; latent_names = names[[2]]
  }
  
  # get the correlation from latents to observed
  slopes_observed = latent_observed_implied(model) %>% data.frame
  
  # compute residual variances (because they need to be factored into the slopes)
  # in matrix form for easy multiplication
  # residual_variance = matrix((1-get_observed_rsq(model, obs_names)), 
  #                            nrow=length(obs_names), 
  #                            ncol=length(latent_names), 
  #                            byrow=F)
  # # residual variances should only be multiplied IF the observed is an indicator of that latent
  # filter_matrix = model@Model@GLIST$lambda; filter_matrix[abs(filter_matrix)>0] = 1
  # residual_variance = filter_matrix * residual_variance + (1-filter_matrix)
  
  # correct for reliability
  #slopes_observed = slopes_observed*residual_variance
  names(slopes_observed) = paste0("slope_", latent_names)
  return(slopes_observed)
}

get_intercepts = function(slopes_allvars, lav_data, latent_names, obs_names) {
  
  obs_means = obs_names %>% purrr::map_dbl(function(x) mean(lav_data[,x])) %>% 
    matrix(nrow=length(obs_names), ncol=length(latent_names), byrow=F)
  lat_means = latent_names %>% purrr::map_dbl(function(x) mean(lav_data[,x])) %>% 
    matrix(nrow=length(obs_names), ncol=length(latent_names), byrow=T)
  intercepts = lat_means - slopes_allvars*obs_means
  names(intercepts) = paste0("intercept_", latent_names)
  return(intercepts)
}

get_observed_rsq = function(model, obs_names) {
  rsq = lavInspect(model, "r2")

  # if there's an exogenous variable, set its rsq to one
  exogen_observed = which(!(obs_names %in% names(rsq)))
  rsq[obs_names[exogen_observed]] = 1
  return(rsq[obs_names])
}


loop_flexplot = function(y, data, cov_allvars, slope, intercept) {
  f = make.formula(y, "Observed | Variable")
  p = flexplot(f, data=data) + abline()
}

# this function will return the correlation between observed and latents
latent_observed_implied = function(model) {
  latent = lavNames(model, type="lv")
  observed = lavNames(model, type="ov")
  
  cor_matrix = lavInspect(model, "cov.all") %>% data.frame
  inter_correlations = cor_matrix[observed, latent]
  return(inter_correlations)
}

