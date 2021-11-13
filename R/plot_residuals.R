



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