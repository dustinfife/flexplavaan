#' Display the model-implied measurement plot
#' 
#' These models plot the latent variable on the Y-axis, and the standardized observed
#' variables on the x axis. Each variable is shown as a separate panel. By default, 
#' the algorithm limits the number of plots to 4, but the user can specify more. 
#' 
#' The model-implied fit is computed using the correlations between the observed/latent
#' variables. Since all variables are standardized before plotting, the correlations become
#' the slopes, and the intercepts are set to zero. 
#'
#' @param model A lavaan object
#' @param latent a string, specifying which latent variable is plotted on the y-axis. 
#' By default, it will display all the variables and return a list of plots. 
#' @param limit The maximum number of observed variables displayed. Defaults to 4.
#' @param sort_slopes Should the plots by sorted by how much their model-implied slopes 
#' deviate from a standard regression model slopes? Defaults to true.  
#' @param ... Other parameters passed to flexplot. 
#'
#' @return Either a ggplot2 plot, or a list of ggplot2 plots
#' @export
implied_measurement = function(model, latent=NULL, limit=4, sort_slopes=T, method="lm", ...) {
  
  # check for name of latent
  check_for_latent(model, latent)
  
  # get long-format, standardized data
  flex_data = prepare_measurement_data(model)
  

  if (is.null(latent)) latent = get_names(model)[[2]]

  plots = latent %>% purrr::map(function(x) latent_flexplot(flex_data, x, limit=limit, sort_slopes=sort_slopes, method,...))
  return(plots)
}

check_for_latent = function(model, latent) {
  if (is.null(latent)) return(NULL)
  if (!(latent %in% lavNames(model, "lv"))) return(stop("The variable you provided is not a latent variable"))
  return(NULL)
}

latent_flexplot = function(flex_data, latent, limit=4, sort_slopes=T, method="lm",...) {
  
  # name the abline parameters
  intercept_name = paste0("intercept_", latent)
  slope_name = paste0("slope_", latent)
  
  # compute actual slopes
  unique_variables = unique(flex_data$Variable)
  actual_slopes = unique_variables %>% 
    purrr::map_dfr(function(x) return_actual_slope(x, latent, flex_data)) %>% 
    mutate(Variable = unique_variables, Actual_slopes = Observed)

  # now compute difference between slopes
  if (sort_slopes) {
    k = (merge(flex_data, actual_slopes, by="Variable"))
    slope_name = rlang::sym(paste0("slope_", latent))
    ordered_differences =  k %>% mutate(Diff = abs(Actual_slopes - !!(slope_name))) %>% 
      group_by(Variable) %>% 
      summarize(mean(Diff)) %>% 
      purrr::set_names(c("Variable", "Diff")) %>% 
      arrange(desc(Diff))
    flex_data$Variable = factor(flex_data$Variable, levels=ordered_differences$Variable, ordered=T)  
    
    # limit the number of plots
    only_plot_these = levels(flex_data$Variable)[1:min(limit, length(flex_data$Variable))]
    flex_data = flex_data %>% filter(Variable %in% only_plot_these)
  }
  
  # now plot it
  ggplot(flex_data, 
         aes_string(x = "Observed", y = latent, group = "1"), ...) +         
    geom_point(...) + 
    facet_wrap(~ Variable) +
    geom_abline(aes_string(intercept=intercept_name, slope=slope_name, group="1"), colour="red", lwd=2) +
    geom_smooth(method=method, formula = y~x, colour="blue") + 
    theme_bw() +
    labs(x="Observed\n(Red = Implied, Blue:=Observed)")
}

vignette("programming", "dplyr")

return_actual_slope = function(name, latent, flex_data) {
  #browser()
  f = as.formula(paste0(latent, "~Observed"))
  d = flex_data %>% filter(Variable == name)
  coef(lm(f, data=d))[2]
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
  slopes_and_intercepts = cbind(slopes_observed, intercepts_observed, Variable=obs_names)
  flex_data = full_join(lav_data_std, slopes_and_intercepts, by="Variable")
  return(flex_data)
}

# this function converts all observed variables to standardized form
standardize_observed = function(model) {
  lav_data = get_all_data(model)
  names = get_names(model)
  obs_names = names[[1]]; latent_names = names[[2]]  
  
  obs_standardized = lav_data %>% 
    transmute_all(scale)
  #lav_data[,obs_names] = obs_standardized[,obs_names]
  return(obs_standardized)
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

get_slopes_actual = function(model) {
  
  
 
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


# this function will return the correlation between observed and latents
latent_observed_implied = function(model) {
  latent = lavNames(model, type="lv")
  observed = lavNames(model, type="ov")
  
  cor_matrix = lavInspect(model, "cor.all") %>% data.frame
  inter_correlations = cor_matrix[observed, latent]
  return(inter_correlations)
}

