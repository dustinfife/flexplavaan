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
implied_measurement = function(model, model2=NULL, latent=NULL, limit=4, sort_slopes=T, method="default", ...) {
  
  # check for name of latent
  check_for_latent(model, latent)

  # check models
  check_models(model, model2)
  
  # get long-format, standardized data
  flex_data = prepare_measurement_data(model)
  if (!is.null(model2)) {
    m1_name = paste0(substitute(model))
    m2_name = paste0(substitute(model2))
    flex_data_two = prepare_measurement_data(model2)
    flex_data_two$model = m2_name
    flex_data$model = m1_name
    flex_data = full_join(flex_data, flex_data_two)
  }
  
  if (is.null(latent)) latent = get_names(model)[[2]]

  plots = latent %>% purrr::map(function(x) latent_flexplot(flex_data, x, limit=limit, sort_slopes=sort_slopes, method,...))
  return(plots)
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

latent_flexplot = function(flex_data, latent, limit=4, sort_slopes=T, method="lm",...) {

  # name the abline parameters
  intercept_name = paste0("intercept_", latent)
  slope_name = paste0("slope_", latent)
  
  ordered_differences = order_flexdata_by_slopes(flex_data, latent, sort_slopes)
  flex_data$Variable = factor(flex_data$Variable, levels=ordered_differences$Variable, ordered=sort_slopes)  
  
  # limit the number of plots
  only_plot_these = levels(flex_data$Variable)[1:min(limit, length(flex_data$Variable))]
  flex_data = flex_data %>% filter(Variable %in% only_plot_these)

  # now plot it
  if ("model" %in% names(flex_data)) {
    p = ggplot(flex_data, 
               aes_string(x = "Observed", y = latent, group = "model", colour="model", shape="model", linetype="model"), ...) 
    if (method=="default") smooth = geom_blank() else smooth = geom_smooth(method=method, formula = y~x, colour="lightgray")
    abline = geom_abline(aes_string(intercept=intercept_name, slope=slope_name, colour="model", linetype="model"), lwd=1) 
    labels = geom_blank()
  } else {
    p = ggplot(flex_data, 
               aes_string(x = "Observed", y = latent, group = "1"), ...) 
    if (method=="default") smooth = geom_smooth(method="loess", formula = y~x, colour="blue") else smooth = geom_smooth(method=method, formula = y~x, colour="blue")
    abline = geom_abline(aes_string(intercept=intercept_name, slope=slope_name, group="1"), colour="red", lwd=2) 
    labels = labs(x="Observed\n(Red = Implied, Blue:=Observed)")
  }
  
  p +
    geom_point(...) + 
    facet_wrap(~ Variable) +
    abline + 
    smooth + 
    theme_bw() +
    labels
}

order_flexdata_by_slopes = function(flex_data, latent, sort_slopes) {

  if (!sort_slopes) {
    return(unique(flex_data$Variable))
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
  #browser()
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

