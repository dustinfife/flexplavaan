#' Visualize a linear lavaan model 
#'
#' This function generates diagnostic plots or model plots of a lavaan object.
#' @param object a lavaan object
#' @param object2 a second lavaan object (optional). This is used to visually compare
#' the fit of two different models. 
#' @param plot what should be plotted? User can specify "diagnostics" or "model"
#' @param ... Other arguments passed to flexplot
#' @import GGally
#' @export
#' @example 
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
visualize.lavaan = function(object, object2=NULL, subset = NULL, plot=c("all", "residuals", "model"), formula = NULL,...){
  plot = match.arg(plot, c("all", "residuals", "model"))
  observed = lavNames(object)
  d = data.frame(lavInspect(object, "data"))
  names(d) = observed
  if (!is.null(subset)) observed = observed[subset]
  if (!is.null(object2)) {
    legend=c(1,2)
  } else {
    legend = NULL
  }
  if (!is.null(object2)) {
    legend=c(1,2)
  } else {
    legend = NULL
  }
  if (plot=="all"){
    ggpairs(d[,observed], legend=legend,
            lower = list(continuous = wrap(viz_diagnostics,fit.lavaan = object, fit.lavaan2 = object2, alpha = .2,invert.map=TRUE, plot="disturbance", ...)),
            upper = list(continuous = wrap(viz_diagnostics,fit.lavaan = object, fit.lavaan2 = object2, alpha = .2, plot="trace", ...)),
            diag = list(continuous = wrap(viz_diagnostics,fit.lavaan = object, fit.lavaan2 = object2, alpha = .2, plot="histogram", ...)))
  } else if (plot == "residuals"){
    ggpairs(d[,observed], legend=legend,
            lower = list(continuous = wrap(viz_diagnostics,fit.lavaan = object, fit.lavaan2 = object2, alpha = .2,invert.map=TRUE, plot="disturbance", ...)),
            upper = NULL,
            diag = list(continuous = wrap(viz_diagnostics,fit.lavaan = object, fit.lavaan2 = object2, alpha = .2, plot="histogram", ...)))    
  }  else {
    ggpairs(d[,observed], legend=legend,
            lower = NULL,
            upper = list(continuous = wrap(viz_diagnostics,fit.lavaan = object, fit.lavaan2 = object2, alpha = .2, plot="trace", ...)),
            diag = list(continuous = wrap(viz_diagnostics,fit.lavaan = object, fit.lavaan2 = object2, alpha = .2, plot="histogram", ...)))     
  }   
} 

#' Visualize a runjags model 
#'
#' This function generates diagnostic plots or model plots of a lavaan object.
#' @param object a runjags object
#' @param object2 a second object (optional). This is used to visually compare
#' the fit of two different models. 
#' @param plot what should be plotted? User can specify "diagnostics" or "model"
#' @param ... Other arguments passed to flexplot
#' @import GGally
#' @import dplyr
#' @export
#' @example 
#' 
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