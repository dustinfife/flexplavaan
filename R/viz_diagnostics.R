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
viz_diagnostics <- function(data, mapping, 
                            fit.lavaan, fit.lavaan2 = NULL, 
                            invert.map=FALSE, alpha=.5, plot=c("trace", "disturbance", "histogram"), ...) {
  #browser()
  plot = match.arg(plot, c("trace", "disturbance", "histogram"))
  ### extract name of latent variables
  observed = lavNames(fit.lavaan)
  latent.names = lavNames(fit.lavaan, type="lv")

  ### plot ggplot object so I can extract the elements
  if (dplyr::as_label(mapping$y) == "NULL" | plot=="histogram"){
    flexplot_form = flexplot::make.formula(dplyr::as_label(mapping$x), "1")
    flexplot::flexplot(flexplot_form, data=data)
  } else {
    #browser()
    variables = c(dplyr::as_label(mapping$x), dplyr::as_label(mapping$y))
    
    ### extract name of variable in aes
    if (invert.map){
      x = variables[2]
      y = variables[1]   
    } else {
      y = variables[2]
      x = variables[1]
    }
    
    if (!all(variables %in% names(data))){
      problem.vars = which(!(variables %in% names(data)))
      var = ifelse(length(problem.vars>1), 
                    paste0("variables ", variables[problem.vars], " are"), 
                    paste0("variable ", variables[problem.vars], " is")) 
      msg = paste0("The ", var, " not in your actual dataset.")
      stop(msg)
    }
    
    if (!all(variables %in% observed)){
      problem.vars = which(!(variables %in% observed))
      var = ifelse(length(problem.vars>1), 
                  paste0("variables ", variables[problem.vars], " are"), 
                  paste0("variable ", variables[problem.vars], " is"))
      msg = paste0("The ", var, " not in your actual dataset.")
      stop(msg)
    }  
    
  
    estimated_fits = estimate_linear_fit(fit.lavaan, x=x, y=y, data)
      new_data = data.frame(x=estimated_fits$x_new, y=estimated_fits$y_new)
      names(new_data) = c(x, y)
      data[,"residuals"] = estimated_fits$residuals
      #browser()
    if (!is.null(fit.lavaan2)){
      estimated_fits = estimate_linear_fit(fit.lavaan2, x, y, data)
      new_data$y2 = estimated_fits$y_new
      data[,"residuals2"] = estimated_fits$residuals      
    }
    
    ### now add to ggplot object
    if (plot=="trace"){
      flexplot_form = flexplot::make.formula(y, x)
      
      if (!is.null(fit.lavaan2)){
        
        n = new_data %>% 
          tidyr::gather(key="Model", value=y, c(!!y,"y2")) %>% 
          dplyr::mutate(Model = factor(Model, levels=c(!!y, "y2"), labels=c("Model 1", "Model 2")))
       p = flexplot::flexplot(flexplot_form, data=data, alpha=alpha, se=F, ...) + 
          geom_line(data=n, aes_string(x,"y", col="Model"))

      } else {
        p = flexplot::flexplot(flexplot_form, data=data, alpha=alpha, se=F, ...) + 
          geom_line(data=new_data, aes_string(x,y), col="red")  
      }
      return(p)
    } else if (plot=="disturbance") {
      ### convert data to long format to make dots different
      if (!is.null(fit.lavaan2)){
        data2 = data[,c(x,"residuals", "residuals2")] %>% tidyr::gather("model", "residuals", c("residuals", "residuals2")) %>% setNames(c(x,"model","residuals"))
        data2$model = factor(data2$model, levels=c("residuals","residuals2"), labels=c("Model 1", "Model 2"))
        f = make.formula("residuals", c(x, "model"))
        p = flexplot::flexplot(f, data=data2, alpha = .2,...) + geom_hline(yintercept = 0) + geom_smooth(se=F)
      } else {
        flexplot_form = flexplot::make.formula("residuals", x)
        p = flexplot::flexplot(flexplot_form, data=data, alpha=alpha, ...) + labs(y=paste0(y, " | latent")) + geom_hline(yintercept=0, col="red")+ geom_smooth()
      }
      return(p)
    } 
  }
}




# fit.mcmc = lvs
# mapping = aes(potions, darkarts)
viz_diagnostics_mcmc <- function(data, mapping, latents, which.latent,
                            fit.mcmc, 
                            invert.map=FALSE, alpha=.5, plot, ...) {
  #browser()
  ### extract name of latent variables
  observed = names(data)
  latent.names = names(latents)[which.latent+1]
  
  ### plot ggplot object so I can extract the elements
  if (dplyr::as_label(mapping$y) == "NULL" | plot=="histogram"){
    flexplot_form = flexplot::make.formula(dplyr::as_label(mapping$x), "1")
    flexplot::flexplot(flexplot_form, data=data)
  } else {
    #browser()

    xy = extract_xy_mapping(mapping, invert.map=FALSE, data = data, observed=names(data), latent=NULL)
    x = xy$x; y = xy$y; 

    x.vals = data.frame(data[,x]); names(x.vals) = x
    y.vals = data.frame(data[,y]); names(y.vals) = y
    latents.to.show = which.latent[which(names(data)==x | names(data) == y)]
    #latent.var = latents[,which(names(data)==x | names(data) == y)]
    visualize_nonlinear(x = x.vals, y=y.vals, latent=latents[,latents.to.show], plot = plot, ...)
  }
}
