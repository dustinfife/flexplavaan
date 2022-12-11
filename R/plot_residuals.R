
return_residual_dataset = function(fitted, max_val = 0.01) {
  obs_names = lavNames(fitted)
  residual_correlations = residuals(fitted, type="cor")$cov 
  
  # string out correlations
  vechs_cors = vechs(residual_correlations)
  pairwise_names = name_vechs(obs_names)
  
  # subset data
  condition = which(abs(vechs_cors)>max_val)
  
  # if there's few values greater than max value, warn the user and change it
  if (length(condition)<3) {
    number_of_vars = length(condition)
    message(paste0("The number of variables you're asking to display, based on your max_val,
                   is only ", number_of_vars, ". I'm going to display 3 variables instead."))
    condition = which(order(abs(vechs_cors))<4)
  }
  # create dataset
  res_d = data.frame(Residual=(vechs_cors[condition]), Correlation = pairwise_names[condition])
  res_d = res_d[order(abs(res_d$Residual), decreasing = T),]
  
  # convert correlations into ordered factor (so axes are consistent)
  res_d$Correlation = factor(res_d$Correlation, levels=res_d$Correlation, ordered=T)
  
  return(res_d)
}

combine_residual_datasets = function(fitted, fitted2=NULL, max_val=.01) {
  
  if (is.null(fitted2)) return(return_residual_dataset(fitted, max_val))

  # get the first dataset, but set maxval to zero
  d_1 = return_residual_dataset(fitted, max_val=0)
  d_2 = return_residual_dataset(fitted2, max_val=0)
  
  # merge the datasets, sort, and gather
  d = merge(d_1, d_2, by="Correlation") %>% 
    filter(abs(Residual.x) > max_val | abs(Residual.y) > max_val ) %>% 
    mutate(average_residual = abs(Residual.x) + abs(Residual.y)) %>% 
    arrange(average_residual) %>% 
    gather(key="Model", value="Residual", Residual.x, Residual.y)

  return(d)
}


#' Plot a "hopper" plot (or residual plot) for a latent variable model
#'
#' These plots show lines representing the absolute value (and negative absolute value)
#' of the residuals from a lavaan model. The dots indicate that actual value of the residual. 
#' These help identify which correlations the model failed to capture. 
#' @param fitted A lavaan object that is fitted
#' @param fitted2 An optional second option we can compare the first model against
#' @param max_val The value at which we stop showing correlations. 
#'
#' @return A hopper plot. 
#' @export
#'
#' @examples
#' hopper_plot(force_fit)
residual_plots = hopper_plot = function(fitted, fitted2=NULL, max_val = 0.01) {

  fitted_l = flexplavaan_to_lavaan(fitted)
  fitted2_l = flexplavaan_to_lavaan(fitted2)
  
  res_d = combine_residual_datasets(fitted_l, fitted2_l, max_val)

  if (is.null(fitted2)) {
    res_d$top = abs(res_d$Residual)
    res_d$bottom = -1*abs(res_d$Residual)
    p = ggplot2::ggplot(res_d, aes(x=Correlation, y=Residual)) +
      geom_line(aes(y=bottom, group=1), col="gray") +
      geom_line(aes(y=top, group=1), col="gray") + 
      geom_point(aes(x=Correlation, y=Residual), size=2) +
      geom_abline(slope=0, intercept=0) + 
      scale_x_discrete(limits = rev(levels(res_d[["Correlation"]])))+
      theme_bw() +
      annotate("text", x=max(res_d$Correlation), y=max(res_d$top), label="Model Underestimates", vjust="inward", hjust="inward") + 
      annotate("text", x=max(res_d$Correlation), y=min(res_d$bottom), label="Model Overestimates", vjust="inward", hjust="inward") + 
      coord_flip()
    return(p)
  }
  
  # rename the levels
  res_d$Model = factor(res_d$Model, levels=c("Residual.x", "Residual.y"), 
                       labels=c(
                         paste0(deparse(substitute(fitted))),
                         paste0(deparse(substitute(fitted2)))))

  # set the bands for the limits
  limits = res_d %>% 
    group_by(Model) %>% 
    mutate(top = abs(Residual),
           bottom = -1*abs(Residual),
           Correlation = factor(Correlation))

  p = ggplot2::ggplot(limits, aes(x=Correlation, y=Residual, group=Model)) +
    geom_line(aes(y=bottom, group=Model, linetype=Model, col=Model), alpha=.4) +
    geom_line(aes(y=top, group=Model, linetype=Model, col=Model), alpha=.4) + 
    geom_point(data=limits, aes(x=Correlation, y=Residual, group=Model, col=Model), size=2) +
    geom_abline(slope=0, intercept=0) + 
    scale_x_discrete(limits = rev(levels(limits[["Correlation"]])))+
    theme_bw() +
    annotate("text", x=max(res_d$Correlation), y=max(res_d$top), label="Model Underestimates", vjust="inward", hjust="inward") + 
    annotate("text", x=max(res_d$Correlation), y=min(res_d$bottom), label="Model Overestimates", vjust="inward", hjust="inward") + 
    coord_flip()
  p
    
}



name_vechs = function(variable_names, collapse=":"){
  combn(variable_names, 
        m=2, 
        FUN = function(x) paste0(x[1], collapse, x[2]))
}