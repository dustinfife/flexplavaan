#' Visualize a linear lavaan model 
#'
#' This function generates diagnostic plots or model plots of a lavaan object.
#' @param object a lavaan object
#' @param object2 a second lavaan object (optional). This is used to visually compare
#' the fit of two different models. 
#' @param subset Either a vector of numbers or variable names indicating which variables
#' should be plotted. Defaults to NULL
#' @param plot what should be plotted? User can specify "all" (default), "ddp", 
#' "trail", "residuals", "measurement", or "latent"
#' @param formula For latent plots, the user can specify a \link[flexplot]{flexplot} formula. 
#' Option is ignored for the other plots
#' @param ... Other arguments passed to flexplot
#' @import GGally
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
                            plot = "all", 
                            formula = NULL,...){

  object_l = flexplavaan_to_lavaan(object)
  object2_l = flexplavaan_to_lavaan(object2)
  
  plot = match.arg(plot, c("all", "ddp", "trail", "disturbance", "residuals", "measurement", "latent"))
 
  if (plot %in% c("all", "trail", "ddp")){
    
    if (is.null(subset)) {
      vars = lavNames(object_l, "ov")
      max_plots = min(length(vars), 4)
      subset = 1:max_plots
    }
    return(plot_scatter_matrix(object_l, object2_l, subset, plot))#%>% modify_model_names(get_and_check_names(NULL, object, object2)))
  } 

  if (plot == "measurement"){

    p = implied_measurement(model = object_l, model2 = object2_l, ...)
    return(p)
  }
  
  if (plot == "latent"){
    p = latent_plot(fitted = object_l, fitted2 = object2_l, ...) 
    return(p)
  }  

  if (plot == "residuals") {
    return(residual_plots(object_l, object2_l))
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
                                 formula = NULL,...){
  object_l = flexplavaan_to_lavaan(object)
  object2_l = flexplavaan_to_lavaan(object2)

  plot = match.arg(plot, c("all", "disturbance", "model", "measurement", "latent", "residual"))

  model_names = get_and_check_names(NULL, object, object2)
  visualize.lavaan(object_l, object2_l, subset, plot, formula,...)
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
visualize.runjags = function(object, summary_bayesian=NULL, data, which.latent=1, object2=NULL, subset = NULL, 
                             plot=c("all", "residuals", "model"), formula = NULL,...){
  
  
  
  # extract summary of bayesian model
  if (is.null(summary_bayesian)) summary_bayesian = summary(object)
  
  #### create factor scores
  factor.scores = export_jags_latents(summary_bayesian)[,-1, drop=F]
  
  
  plot = match.arg(plot,c("all", "residuals", "model"))
  
  
  ### extract name of latent variables
  observed = extract_variables_from_jags_object(object)[subset]
  latent.names = names(factor.scores)
  
  # make sure which.latent equals the number of observed variables
  if (length(which.latent) != length(observed)) {
    warning("The length of 'which.latent' is not the same as the number of observed variables. I'm going to modify that so it works.")
    which.latent = rep(which.latent, times=length(observed)/length(which.latent))
  }
  
  # get rid of missing values
  data = data %>% select(all_of(observed)) 
  
  if (plot=="all"){
    
    #viz_diagnostics_mcmc(data[,observed], mapping, factor.scores, plot="trace")
    return(ggpairs(data[,observed],
            lower = list(continuous = wrap(viz_diagnostics_mcmc,latents = factor.scores, 
                                          which.latent=which.latent, alpha = .2, plot="disturbance", ...)),
            upper = list(continuous = wrap(viz_diagnostics_mcmc,latents = factor.scores, 
                                           which.latent=which.latent, alpha = .2, plot="trace", ...)),
            diag  = list(continuous = wrap(viz_diagnostics_mcmc,latents = factor.scores, 
                                           which.latent=which.latent, alpha = .2, plot="histogram", ...))))
  }
    
  if (plot == "residuals"){
    return(ggpairs(data[,observed], legend=legend,
            lower = list(continuous = wrap(viz_diagnostics_mcmc,latents = factor.scores, 
                                           which.latent=which.latent,alpha = .2,invert.map=TRUE, plot="disturbance", ...)),
            upper = NULL,
            diag  = list(continuous = wrap(viz_diagnostics_mcmc,latents = factor.scores, 
                                           which.latent=which.latent,alpha = .2, plot="histogram", ...))))
    return(
    ggpairs(data[,observed], legend=legend,
            lower = NULL,
            upper = list(continuous = wrap(viz_diagnostics_mcmc,latents = factor.scores, 
                                           alpha = .2, plot="trace", ...)),
            diag  = list(continuous = wrap(viz_diagnostics_mcmc,latents = factor.scores, 
                                           alpha = .2, plot="histogram", ...))))
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