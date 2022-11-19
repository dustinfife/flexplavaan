# see if variances are negative
find_negative_variances = function(fitted) {
  variances = diag(lavInspect(fitted, what="std")$theta)
  negative_variances = names(which(variances<0))
  if (length(negative_variances)>0) {
    stop(paste0("Sorry, but you can't plot a measurement plot when there are negative variances. 
                By the way, it was the variable(s) ", paste0(negative_variances, collapse=","), 
                " that caused the problem."))
  }
  return(NULL)
}

# this creates the scatterplot data for implied measurement plots
prepare_measurement_data = function(model, model2=NULL) {
  # get names
  names = get_names(model)
  obs_names = names[[1]]; latent_names = names[[2]]
  
  # load dataset
  lav_data = standardize_observed(model)
  
  # estimate matrix of slopes/intercepts (between latent and observed)
  slopes_observed = get_slopes(model, obs_names, latent_names)
  intercepts_observed = get_intercepts(slopes_observed, model)
  
  # convert from wide to long format
  lav_data_std = lav_data %>% 
    pivot_longer(cols=all_of(obs_names), names_to="Variable", values_to="Observed") %>% 
    data.frame %>% 
    purrr::set_names(c(latent_names, "Variable", "Observed"))
  
  # merge the intercept data with the actual data
  slopes_and_intercepts = cbind(slopes_observed, intercepts_observed, Variable=obs_names)
  flex_data = full_join(lav_data_std, slopes_and_intercepts, by="Variable")

  if (is.null(model2)) return(flex_data)
  
  m1_name = paste0(substitute(model))
  m2_name = paste0(substitute(model2))
  flex_data_two = prepare_measurement_data(model2)
  flex_data_two$model = m2_name
  flex_data$model = m1_name
  common_names = names(flex_data)[names(flex_data) %in% names(flex_data_two)]
  flex_data = full_join(flex_data, flex_data_two[,common_names], by=common_names)
  return(flex_data)
  
}

# this function sorts the implied measurement data plots so that the most descrepant are shown first
order_flexdata_by_slopes = function(flex_data, latent, sort_slopes) {
  
  if (!sort_slopes) {
    # this must be returned as a list so later when I call x$Variable it doesn't 
    # screw things up
    return(list(Variable = unique(flex_data$Variable)))
  }
  
  # compute actual slopes if they didn't provide a second model
  if ("model" %in% names(flex_data)) {
    # figure out order based on differences in slopes
    slope_name = rlang::sym(paste0("slope_", latent))
    model_names = unique(flex_data$model)
    ordered_differences = flex_data %>% 
      group_by(model, !!(slope_name), Variable) %>% 
      summarize(slope = mean(!!(slope_name))) %>% 
      pivot_wider(id_cols=Variable, names_from = model, values_from = slope) %>% 
      mutate(Diff := abs(!!(rlang::sym(model_names[1])) - !!(rlang::sym(model_names[2])))) %>% 
      select(Variable, Diff) %>% 
      arrange(desc(Diff))
    return(ordered_differences)
  } 
  
  # if they didn't provide a second model, compute difference between actual slopes and model slopes
  unique_variables = unique(flex_data$Variable)
  actual_slopes = unique_variables %>% 
    purrr::map_dfr(function(x) return_actual_slope(x, latent, flex_data)) %>% 
    mutate(Variable = unique_variables, Actual_slopes = Observed)
  k = (merge(flex_data, actual_slopes, by="Variable"))
  slope_name = rlang::sym(paste0("slope_", latent))
  ordered_differences =  k %>% mutate(Diff = abs(Actual_slopes - !!(slope_name))) %>% 
    group_by(Variable) %>% 
    summarize(mean(Diff)) %>% 
    purrr::set_names(c("Variable", "Diff")) %>% 
    arrange(desc(Diff))
  return(ordered_differences)
  
}

# this function converts all observed variables to standardized form
standardize_observed = function(model, model2=NULL) {
  lav_data = get_all_data(model)
  names = get_names(model)
  obs_names = names[[1]]; latent_names = names[[2]]  
  
  obs_standardized = lav_data %>% 
    transmute_all(scale)
  
  if (is.null(model2)) return(obs_standardized)
  m1_name = paste0(substitute(model, sys.frame(1)))
  m2_name = paste0(substitute(model2, sys.frame(1)))
  obs_standardized_2 = get_all_data(model2) %>% 
    transmute_all(scale) %>% 
    mutate(model=m2_name)
  obs_standardized$model = m1_name
  return(data.frame(rbind(obs_standardized, obs_standardized_2)))
}

check_for_latent = function(model, latent) {
  if (is.null(latent)) return(NULL)
  if (!(latent %in% lavNames(model, "lv"))) return(stop("The variable you provided is not a latent variable"))
  return(NULL)
}

return_actual_slope = function(name, latent, flex_data) {
  
  f = as.formula(paste0(latent, "~Observed"))
  d = flex_data %>% filter(Variable == name)
  coef(lm(f, data=d))[2]
}


get_slopes = function(model, obs_names=NULL, latent_names=NULL) {

  if (is.null(obs_names) | is.null(latent_names)) {
    # get names
    names = get_names(model)
    obs_names = names[[1]]; latent_names = names[[2]]
  }
  
  # get the correlation from latents to observed
  slopes_observed = latent_observed_implied(model) %>% data.frame
  names(slopes_observed) = paste0("slope_", latent_names)
  return(slopes_observed)
}

get_intercepts = function(slopes_allvars, model) {
  lav_data = standardize_observed(model)
  names = get_names(model)
  obs_names = names[[1]]; latent_names = names[[2]]
  obs_means = obs_names %>% purrr::map_dbl(function(x) mean(lav_data[,x])) %>% 
    matrix(nrow=length(obs_names), ncol=length(latent_names), byrow=F)
  lat_means = latent_names %>% purrr::map_dbl(function(x) mean(lav_data[,x])) %>% 
    matrix(nrow=length(obs_names), ncol=length(latent_names), byrow=T)
  intercepts = lat_means - slopes_allvars*obs_means
  names(intercepts) = paste0("intercept_", latent_names)
  return(intercepts)
}


# this function will return the correlation between observed and latents
latent_observed_implied = function(model) {
  latent = lavNames(model, type="lv")
  observed = lavNames(model, type="ov")
  
  cor_matrix = lavInspect(model, "cor.all") %>% data.frame
  inter_correlations = cor_matrix[observed, latent]
  return(inter_correlations)
}

find_common_latent = function(model1, model2, latent=NULL) {
  
  # if user specifies latent, return latent
  if (!is.null(latent)) return(latent)
  
  # return the latent variable if there's just one model
  if (is.null(model2)) return(rank_worst_fitting_latents(model1)[1])
  
  
  latents = lavNames(model1, "lv")
  if (length(latents)==1) return(latents)

  # find latent variables common in both models
  
  ranks_1 = rank_worst_fitting_latents(model1) %>% data.frame %>% mutate(rank = 1:dplyr::n())
  ranks_2 = rank_worst_fitting_latents(model2) %>% data.frame %>% mutate(rank = 1:dplyr::n())
  
  matches = ranks_2$`.` %in% ranks_1$`.`
  if (all(!(matches))) { stop("Sorry, there are no latent variables in common between your two models.")}
  returned_variable = ranks_2$`.`[matches][1]
  return(returned_variable)
}

rank_worst_fitting_latents = function(model) {

  latents = lavNames(model, "lv")
  slope_name = paste0("slope_", latents)
  
  # see if there's only one latent variable
  if (length(latents) == 1) return(latents)
  
  # compute the average slope descrepancy by latent variable
  flex_data = prepare_measurement_data(model)
  ranks_latents = flex_data %>% 
    mutate(across(all_of(slope_name), ~ abs(.x - Observed))) %>% # computes difference between observed and implied slope
    summarize(across(all_of(slope_name), .fns=mean, na.rm=T))
  return(names(suppressWarnings(sort(ranks_latents[1,]))) %>% gsub_piped("slope_", ""))
}



