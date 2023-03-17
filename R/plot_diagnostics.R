#' Visualize Diagnostics for a lavaan object
#'
#' This function visualizes pairwise relationships between items in a lavaan
#' object. The user can specify whether to display "trace" or "disturbance" plots 
#' Trace plots will show the scatterplot between pairs of items (defined via the mapping argument)
#' with the model-implied fit overlaying the raw data. If the line fails to pass through the more
#' concentrated parts of the data, this will indicate misfit. The disturbance plots (or
#' disturbance-dependence plots) will show the relationship between the two items after 
#' removing the model-implied effects. If any relationship remains, it suggests the model failed
#' to capture important information. 
#' @param data The dataset containing the raw (item) observations
#' @param mapping A ggplot-defined mapping (e.g., \code{aes(x=x, y=y)}).
#' @param fit.lavaan A fitted \code{\link{lavaan::lavaan}} object
#' @param alpha The transparency of the datapoints in the scatterplot. Defaults to 0.5
#' @param plot One of the following: "trace" or "disturbance" 
#' @param invert.map Should the x/y axes be flipped in the mapping? This is important when using viz_diagnostics
#' within \code{\link{GGally::ggpairs}}, otherwise the residuals don't match the "trace" plots. 
#' @param ... Other arguments passed to \code{\link{ggplot2::ggplot}}
#'
#' @return a plot of the desired relationship
#' @export
#' @import flexplot ggplot2
#'
#' @examples
#' # fit a correctly specified model
#' require(lavaan)
#' data("correct_large")
#' data("crossloadings_large")
#' 
#' model = "
#' f1 =~ x1 + x2 + x3
#' f2 =~ y1 + y2 + y3
#' f1 ~ f2
#' "
#'   fit.lavaan = cfa(model, data=correct_large)
#'   viz_diagnostics(data = correct_large, mapping = aes(x1, x2), fit.lavaan, plot="trace")
#'   viz_diagnostics(data = correct_large, mapping = aes(x1, x2), fit.lavaan, plot="disturbance")
#'   viz_diagnostics(data = correct_large, mapping = aes(x1, y2), fit.lavaan, plot="trace")
#'   viz_diagnostics(data = correct_large, mapping = aes(x1, y2), fit.lavaan, plot="disturbance")  
#'   
#'   viz_diagnostics(data = crossloadings_large, mapping = aes(x1, x2), fit.lavaan, plot="trace")
#'   viz_diagnostics(data = crossloadings_large, mapping = aes(x1, x2), fit.lavaan, plot="disturbance")
#'   viz_diagnostics(data = crossloadings_large, mapping = aes(x1, y1), fit.lavaan, plot="trace")
#'   viz_diagnostics(data = crossloadings_large, mapping = aes(x1, y1), fit.lavaan, plot="disturbance")  
#' 
#' 
#' data("mugglevwizard")
#' ### fit measurement model
#' model_witch = '
#' witch =~ a*strange + b*relatives + c*wingardium
#' 
#' witch ~~ witch
#' '
#' require(lavaan)
#' mod = cfa(model_witch, data=mugglevwizard)
#' viz_diagnostics(data = mugglevwizard, mapping = aes(wingardium, strange), fit.lavaan = mod, plot="trace")
#' viz_diagnostics(data = mugglevwizard, mapping = aes(wingardium, relatives), fit.lavaan = mod, plot="trace")
#' expect_error(viz_diagnostics(data = mugglevwizard, mapping = aes(wingardium, darkarts), fit.lavaan = mod, plot="trace"))
#' 
#' 
#' model_witch = "
#' witch =~ a*strange + b*relatives + c*wingardium
#' owl =~ d*darkarts + e*potions + f*history
#' witch ~ owl
#' witch ~~ witch
#' owl ~~ owl
#' "
#' mod = cfa(model_witch, data=mugglevwizard)
#' viz_diagnostics(data = mugglevwizard, mapping = aes(wingardium, darkarts), fit.lavaan = mod, plot="trace")
#' viz_diagnostics(data = mugglevwizard, mapping = aes(potions, darkarts), fit.lavaan = mod, plot="disturbance")
#' viz_diagnostics(data = mugglevwizard, mapping = aes(potions, darkarts), fit.lavaan = mod, plot="disturbance")
viz_diagnostics <- function(data, mapping, 
                            fit.lavaan, fit.lavaan2 = NULL, 
                            plot=c("trace", "disturbance", "histogram"), ...) {

  plot = match.arg(plot, c("trace", "disturbance", "histogram"))
  
  ### extract name of latent/observed variables
  variables = invert_aes_mapping(mapping, invert.map = F)

  # check for errors
  viz_diagnostics_error_check(variables, fit.lavaan)
  
  # return histogram
  if (dplyr::as_label(mapping$y) == "NULL" | plot=="histogram") {
    return(diagnostics_histogram(fit.lavaan, mapping, ...))
  }  
    
  if (plot=="trace"){
    return(diagnostics_trace(fit.lavaan, fit.lavaan2, mapping, ...))
  } 
  
  else if (plot=="disturbance") {
    return(diagnostics_disturbance(fit.lavaan, fit.lavaan2, mapping, ...))
  } 
}

diagnostics_disturbance = function(fit.lavaan, fit.lavaan2=NULL, mapping, ...) {
  
  # get variable strings
  variables = invert_aes_mapping(mapping, invert.map = T)
  x = variables[1]
  y = variables[2]
  flexplot_form = flexplot::make.formula(y, x)
  
  # get necessary data (maybe split this up?)
  d          = viz_diagnostics_get_data(fit.lavaan, fit.lavaan2, variables) 
  data       = d$data       # scatterplot data
  new_data   = d$new_data   # the fitted line
  y2_name    = d$y2_name    # the randomly chosen name for the fit of the second line
  resid_name = d$resid_name # the randomly chosen name for the fit in the ddps
  
  ### convert data to long format to make dots different
  if (!is.null(fit.lavaan2)){
    # gather scatterplot data so I can plot two lines
    data2 = data[,c(x,"residuals", resid_name)] %>% 
      tidyr::gather("model", "residuals", c("residuals", all_of(resid_name))) %>% 
      setNames(c(x,"model","residuals"))
    data2$model = factor(data2$model, levels=c("residuals",resid_name), labels=c("Model 1", "Model 2"))
    f = make.formula("residuals", c(x, "model"))
    p = flexplot::flexplot(f, data=data2,...) + geom_hline(yintercept = 0) 
    return(p)
  } 
  
  flexplot_form = flexplot::make.formula("residuals", x)
  p = flexplot::flexplot(flexplot_form, data=data, ...) + 
    labs(y=paste0(y, " | latent")) + 
    geom_hline(yintercept=0, col="red")
  return(p)
}

diagnostics_trace = function(fit.lavaan, fit.lavaan2=NULL, mapping, ...) {
  
  # get variable strings
  variables = invert_aes_mapping(mapping, invert.map = F)
  x = variables[1]
  y = variables[2]
  flexplot_form = flexplot::make.formula(y, x)

  # get necessary data (maybe split this up?)
  d      = viz_diagnostics_get_data(fit.lavaan, fit.lavaan2, variables) 
  data     = d$data     # scatterplot data
  new_data = d$new_data # the fitted line
  y2_name  = d$y2_name  # the randomly chosen name for the fit of the second line

  if (!is.null(fit.lavaan2)){
    # convert to long format so I can plot two lines
    n = new_data %>% 
      tidyr::gather(key="Model", value=y, all_of(c(!!y,y2_name))) %>% 
      dplyr::mutate(Model = factor(Model, levels=c(!!y, y2_name), labels=c("Model 1", "Model2")))
    p = flexplot::flexplot(flexplot_form, data=data, se=F, suppress_smooth = T, ...) + 
      geom_line(data=n, aes_string(x,"y", col="Model"))
    return(p)
  } 
  
  # second line is just the fit of the model
  p = flexplot::flexplot(flexplot_form, data=data, se=F, ...) + 
      geom_line(data=new_data, aes_string(x,y), col="red") + labs(title="Trace Plot")

  return(p)
}


### plot ggplot object so I can extract the elements
diagnostics_histogram = function(fit.lavaan, mapping, ...){
  observed = lavNames(fit.lavaan)
  # output residuals
  i = which(observed == as_label(mapping$x))
  d = data.frame(residual_from_latents(i, fit.lavaan))
  names(d) = as_label(mapping$x)
  
  flexplot_form = flexplot::make.formula(dplyr::as_label(mapping$x), "1")
  return(flexplot::flexplot(flexplot_form, data=d,...))
}



# fit.mcmc = lvs
# mapping = aes(potions, darkarts)
viz_diagnostics_mcmc <- function(data, mapping, latents, which.latent,
                                 fit.mcmc, 
                                 invert.map=FALSE, alpha=.5, plot, ...) {
  
  ### extract name of latent variables
  observed = names(data)
  latent.names = names(latents)[which.latent]
  
  ### plot ggplot object so I can extract the elements
  if (dplyr::as_label(mapping$y) == "NULL" | plot=="histogram"){
    flexplot_form = flexplot::make.formula(dplyr::as_label(mapping$x), "1")
    flexplot::flexplot(flexplot_form, data=data)
  } else {
    
    
    xy = extract_xy_mapping(mapping, invert.map=FALSE, data = data, observed=names(data), latent=NULL)
    x = xy$x; y = xy$y; 
    
    x.vals = data.frame(data[,x]); names(x.vals) = x
    y.vals = data.frame(data[,y]); names(y.vals) = y
    latents.to.show = which.latent[which(names(data)==x | names(data) == y)]
  
    #latent.var = latents[,which(names(data)==x | names(data) == y)]
    visualize_nonlinear(x = x.vals, y=y.vals, latent=latents[,latents.to.show], plot = plot, ...)
  }
}

visualize_nonlinear = function(x,y,latent, plot){
  
  x.names = names(x)
  y.names = names(y)
  data = data.frame(x,y)
  names(data) = c(x.names, y.names)
  
  newpred = nonlinear_prediction(x, y, latent) 
  
  newpred = data.frame(newpred)
  form = flexplot::make.formula(y.names, x.names)
  if (plot=="trace"){
    flexplot(form, data=data) +
      geom_line(data=newpred, aes(x, y), col="red", size=1.5)    
  } else if (plot == "disturbance"){
    data$residuals = unlist(y - newpred$y)
    flexplot(flexplot::make.formula("residuals",x.names), data=data) +
      geom_hline(yintercept = 0, col="red", size=1.5)
  }
  
}

