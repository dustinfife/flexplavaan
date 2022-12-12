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
implied_measurement = function(model, model2=NULL, latent=NULL, limit=4, sort_slopes=T, plot_residuals=T, ...) {

  model_l = flexplavaan_to_lavaan(model)
  model2_l = flexplavaan_to_lavaan(model2)
  
  
  # make sure the models are actually a model
  if (class(model_l) != "lavaan" ) stop("Your model needs to be a lavaan object.")
  if (class(model2_l) != "lavaan" & class(model2_l) != "NULL") stop("Your second model needs to be a lavaan object.")

  # check models
  #check_models(model_l, model2_l)
  latent = find_common_latent(model_l, model2_l, latent)

  # get long-format, standardized data
  flex_data = prepare_measurement_data(model_l, model2_l)
  plots = latent_flexplot(flex_data, latent, limit=limit, sort_slopes=sort_slopes, ...)
  return(plots)
  
}


# this function NEEDS to be split up because I use it in plot_modifiers
latent_flexplot = function(flex_data, latent, limit=4, sort_slopes=T, plot_residuals=T,...) {

  # name the abline parameters
  intercept_name = paste0("intercept_", latent)
  slope_name = paste0("slope_", latent)
  
  ordered_differences = order_flexdata_by_slopes(flex_data, latent, sort_slopes)
  flex_data$Variable = factor(flex_data$Variable, levels=ordered_differences$Variable, ordered=sort_slopes)  
  
  # limit the number of plots
  only_plot_these = levels(flex_data$Variable)[1:min(limit, length(flex_data$Variable))]
  flex_data = flex_data %>% filter(Variable %in% only_plot_these)
  
  # change method if they supply it
  if ("method" %in% names(list(...))) {
    method = list(...)$method
  } else {
    method = "loess"
  }
  
  # if they're doing residuals...
  if (plot_residuals) {
    outcome = paste0("residual_", latent)
  } else {
    outcome = latent
  }
  
  
  # now plot it
  if ("model" %in% names(flex_data)) {
    p = ggplot(flex_data, 
               aes_string(x = "Observed", y = outcome, group = "model", colour="model", shape="model", linetype="model"), ...) 
    smooth = geom_blank() 
    abline = geom_abline(aes_string(intercept=intercept_name, slope=slope_name, colour="model", linetype="model"), lwd=1) 
    labels = geom_blank()
  } else {
    p = ggplot(flex_data, 
               aes_string(x = "Observed", y = outcome, group = "1"), ...) 
    smooth = geom_smooth(method=method, formula = y~x, colour="blue") 
    abline = geom_abline(aes_string(intercept=intercept_name, slope=slope_name, group="1"), colour="red", lwd=2) 
    labels = labs(x="Observed\n(Red = Implied, Blue:=Observed)")
  }
  
  # again, if they're doing residuals...
  if (plot_residuals) {
    abline = geom_hline(yintercept=0, colour = "red", lwd=2)
    y_label = paste0(latent, " | Model")
  } else {
    y_label = latent
  }
  
  p +
    suppressWarnings(geom_point(...)) + 
    facet_wrap(~ Variable) +
    abline + 
    smooth + 
    theme_bw() +
    labs(y=y_label)
    

}

return_plot_type_implied_measurement = function(flex_data, latent, residuals=T) {
  
}


