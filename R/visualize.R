#' Visualize a linear lavaan model 
#'
#' This function generates diagnostic plots or model plots of a lavaan object.
#' @param object a lavaan object
#' @param object2 a second lavaan object (optional). This is used to visually compare
#' the fit of two different models. 
#' @param subset Either a vector of numbers or variable names indicating which variables
#' should be plotted. Defaults to NULL
#' @param plot what should be plotted? User can specify "all" (default), "disturbance", 
#' "model", "measurement", "latent", or "residual"
#' @param formula For latent plots, the user can specify a \link[flexplot]{flexplot} formula. 
#' Option is ignored for the other plots
#' @param sort_plots Should the axes be sorted according to the size of the residuals? Setting to 
#' TRUE (default) will plot the variables with the largest residuals first
#' @param model_names What should the legend be named for the two lines? Defaults to NULL. 
#' @param ... Other arguments passed to flexplot
#' @import GGally
#' @importFrom flexplot visualize
#' @export
#' @aliases visualize visualize.lavaan
#' @examples
#' require(lavaan)
#' data("correct_small")
#' 
#' model = "
#' f1 =~ x1 + x2 + x3
#' f2 =~ y1 + y2 + y3
#' f1 ~ f2
#' "
#'   fit.lavaan = cfa(model, data=correct_small)
#'   visualize(fit.lavaan)
visualize.lavaan = function(object, object2=NULL, 
                            subset = NULL, 
                            plot=c("all", "disturbance", "model", "measurement", "latent", "residual"), 
                            formula = NULL,
                            sort_plots = TRUE,
                            model_names = NULL,...){

  object_l = flexplavaan_to_lavaan(object)
  object2_l = flexplavaan_to_lavaan(object2)
  
  plot = match.arg(plot, c("all", "disturbance", "model", "measurement", "latent", "residual"))
  observed = lavNames(object_l)
  d = data.frame(lavInspect(object_l, "data"))
  names(d) = observed
  
  ## sort the axes
  condition = sort_plots & plot %in% c("all", "disturbance", "model")
  variable_order = ifelse(rep(condition, times=length(observed)), block_model_residuals(object_l), 1:length(observed))
  d = d[,variable_order]
  observed = observed[variable_order]
  
  # specify subsets
  observed = get_subset(subset, observed)
  if (!is.null(object2)) {
    legend=c(1,2)
  } else {
    legend = NULL
  }


  ## get names
  nms = get_and_check_names(model_names, object, object2)

  if (plot=="all"){
    p = ggpairs(d[,observed], legend=legend,
            lower = list(continuous = wrap(viz_diagnostics,fit.lavaan = object_l, fit.lavaan2 = object2_l, alpha = .2,invert.map=TRUE, plot="disturbance", label_names=nms,...)),
            upper = list(continuous = wrap(viz_diagnostics,fit.lavaan = object_l, fit.lavaan2 = object2_l, alpha = .2, plot="trace", label_names=nms, ...)),
            diag = list(continuous = wrap(viz_diagnostics,fit.lavaan = object_l, fit.lavaan2 = object2_l, alpha = .2, plot="histogram", label_names=nms, ...)))
    if (is.null(object2)) {
      p = p + labs(title="Trail/DDP Plots", subtitle="Red=Implied, Blue=Observed")
    }  
    return(p)
  } 
  
  if (plot == "disturbance"){
    p = ggpairs(d[,observed], legend=legend,
            lower = list(continuous = wrap(viz_diagnostics,fit.lavaan = object_l, fit.lavaan2 = object2_l, alpha = .2,invert.map=TRUE, plot="disturbance", label_names=nms, ...)),
            upper = NULL,
            diag = list(continuous = wrap(viz_diagnostics,fit.lavaan = object_l, fit.lavaan2 = object2_l, alpha = .2, plot="histogram", label_names=nms, ...)))    
    if (is.null(object2_l)) {
      p = p + labs(title="DD Plots", subtitle="Red=Implied, Blue=Observed")
    }  
    return(p)
  }  
  
  if (plot == "measurement"){
    p = implied_measurement(object_l, object2_l, ...)  
    return(p)
  }
  
  if (plot == "latent"){
    
    # get length of endogenous variables to make sure we can do it
    if (length(get_endogenous_names(object_l))<2) stop("You cannot do a latent plot when there's less than two endogenous variables.")
    p = latent_plot(object, object2, formula, model_names=model_names, ...)  
    return(p)
  }  

  if (plot == "model") {
    p = ggpairs(d[,observed], legend=legend,
            lower = NULL,
            upper = list(continuous = wrap(viz_diagnostics,fit.lavaan = object_l, fit.lavaan2 = object2_l, 
                                           alpha = .2, plot="trace", label_names=nms, ...)),
            diag = list(continuous = wrap(viz_diagnostics,fit.lavaan = object_l, fit.lavaan2 = object2_l, 
                                          alpha = .2, plot="histogram", label_names=nms, ...)))     
    p = p + labs(title="Trail Plots", subtitle="Red=Implied, Blue=Observed")
    return(p)
    
  }
  
  if (plot == "residuals") {
    p = residual_plots(object_l, object2_l)
  }
  
  
  
} 

#' Visualize a flexplavaan model
#'
#' Visualize a flexplavaan model
#' @aliases visualize visualize.flexplavaan
#' @export
visualize.flexplavaan = function(object, object2=NULL, 
                                 subset = NULL, 
                                 plot = "all", 
                                 formula = NULL,
                                 sort_plots = TRUE,
                                 model_names = NULL,...){
  object_l = flexplavaan_to_lavaan(object)
  object2_l = flexplavaan_to_lavaan(object2)

  plot = match.arg(plot, c("all", "disturbance", "model", "measurement", "latent", "residual"))

  model_names = get_and_check_names(model_names, object, object2)
  visualize.lavaan(object, object2, subset, plot, formula, sort_plots, model_names = model_names,...)
}  

#' Visualize a runjags model 
#'
#' This function generates diagnostic plots or model plots of a lavaan object.
#' @param object a runjags object
#' @param object2 a second object (optional). This is used to visually compare
#' the fit of two different models. 
#' @param plot what should be plotted? User can specify "diagnostics" or "model"
#' @param ... Other arguments passed to flexplot
#' @aliases visualize visualize.runjags
#' @import GGally
#' @import dplyr
#' @export
# object = readRDS(file="data/hogwarts_summary.rds")
# data(hogwarts_survival)
# data = hogwarts_survival
# which.latent = c(1,1,1,2,2,2)
# mapping = aes(potions, darkarts)
# visualize.runjags(object, data, which.latent=c(1,1,1,2,2,2))
#viz_diagnostics_mcmc(data, mapping, latents=factor.scores, plot="disturbance")
visualize.runjags = function(object, data, which.latent=c(1,1), object2=NULL, subset = NULL, plot=c("all", "residuals", "model"), formula = NULL,...){
  
  
  #### create factor scores
  factor.scores = export_jags_latents(object)[,-1]
  
  #browser()
  plot = match.arg(plot,c("all", "residuals", "model"))
  
  ### extract name of latent variables
  observed = names(data)
  latent.names = names(factor.scores)
  if (!is.null(subset)) {
    observed = names(data)[subset]
  } else {
    observed = names(data)
  }
  
  
  
  
  if (plot=="all"){
    
    #viz_diagnostics_mcmc(data[,observed], mapping, factor.scores, plot="trace")
    ggpairs(data[,observed],
            lower = list(continuous = wrap(viz_diagnostics_mcmc,latents = factor.scores, which.latent=which.latent, alpha = .2, plot="disturbance", ...)),
            upper = list(continuous = wrap(viz_diagnostics_mcmc,latents = factor.scores, which.latent=which.latent,alpha = .2, plot="trace", ...)),
            diag = list(continuous = wrap(viz_diagnostics_mcmc,latents = factor.scores, which.latent=which.latent,alpha = .2, plot="histogram", ...)))
  } else if (plot == "residuals"){
    ggpairs(data[,observed], legend=legend,
            lower = list(continuous = wrap(viz_diagnostics_mcmc,latents = factor.scores, which.latent=which.latent,alpha = .2,invert.map=TRUE, plot="disturbance", ...)),
            upper = NULL,
            diag = list(continuous = wrap(viz_diagnostics_mcmc,latents = factor.scores, which.latent=which.latent,alpha = .2, plot="histogram", ...)))
  }  else {
    ggpairs(data[,observed], legend=legend,
            lower = NULL,
            upper = list(continuous = wrap(viz_diagnostics_mcmc,latents = factor.scores, alpha = .2, plot="trace", ...)),
            diag = list(continuous = wrap(viz_diagnostics_mcmc,latents = factor.scores, alpha = .2, plot="histogram", ...)))
  }         
  
} 

#' Wizard/Witch Propensity Scores for 500 Students at Hogwarts 
#'
#' This dataset contains scores on various measures of wizarding ability
#' and their subsequent OWL scores at Hogwarts
#'
#' @format A data frame with 500 rows and 7 variables:
#' \describe{
#'   \item{strange}{Strange occurance scores on a 40 item survey, completed by an independent observer}
#'   \item{telekenesis}{Number of occurances reported by potential witch/wizard, self-reported}
#'   \item{wingardium}{Number of seconds the student was able to levitate an object using the spell "wingardium leviosa"}
#'   \item{darkarts}{Score at the final year of Hogwarts in "Defense Against the Dark Arts"}
#'   \item{potions}{Score at the final year of Hogwarts in "Potions"}
#'   \item{history}{Score at the final year of Hogwarts in "History of Magic"}
#'   \item{mugglevwizard}{Indicator variable that identifies each student as muggle or witch/wizard}
#'   
#' }
#' @source These data are proprietary and obtained through the school of Hogwarts. 
"mugglevwizard"