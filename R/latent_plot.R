#fitted = fit_twofactor
# latent_plot(fit_bollen, formula = Eta2 ~ Eta1)
# latent_plot(fit_bollen)
latent_plot = function(fitted, formula = NULL) {
  
  latent_names = lavaan::lavNames(fitted, type="lv")
  latent_predicted = data.frame(lavPredict(fitted))

  ### estimate standard errors
  se_data = 1:length(latent_names) %>% 
    purrr::map(~estimate_standard_errors(.x,fitted)$sd_imp) %>%  # returns list of se for each latent var
    data.frame
  names(se_data) = paste0("se_", latent_names)
  
  ### get flexplot formulae
  if (is.null(formula)) { 
    formula = beta_to_flexplot(fitted, latent_predicted)
    plot_list = formula %>% purrr::map(~latent_plot_only(.x, latent_predicted, se_data))
    return(plot_list)
  }
  
  plot = latent_plot_only(formula, latent_predicted, se_data)
  return(plot)

}

# dv = dvs[1]
# i=1
# formula = forms
# data=latent_predicted
latent_plot_only = function(f, data, se_data) {
  #cat(f)
  #browser()
  xvar = all.vars(f)[-1]
  yvar = all.vars(f)[1]
  data = cbind(data, se_data)
  p = flexplot(f, data, se=F, ghost.line="red", alpha=0) + 
    ggforce::geom_ellipse(aes_string(x0 = xvar[1], y0 = yvar, a = paste0("se_", xvar[1]), b = paste0("se_", yvar), angle=0), color=rgb(0,0,0,.1))
  return(p)
}

#fitted = fit_bollen
#beta_to_flexplot(fit_bollen, data.frame(lavPredict(fit_bollen)))
beta_to_flexplot = function(fitted, data, return_dvs=FALSE) {
  
  latent_names = lavaan::lavNames(fitted, type="lv")
  
  # get the beta matrix (which is the path coefficients between latent variables)
  beta_matrix = fitted@Model@GLIST$beta
  dvs = which(rowSums(beta_matrix)>0)
  
  if(return_dvs) return(dvs)
  
  model_formulas = dvs %>% purrr::map(~
                       flexplot:::make_flexplot_formula(
                         predictors = latent_names[get_dv_iv(.x, beta_matrix)],
                         outcome = latent_names[.x], 
                         data=data
                       )
                    )
  
  return(model_formulas)
}


#get_dv_iv(2, beta_matrix)
get_dv_iv = function(i, beta_matrix){
  which(beta_matrix[i,]>0)
}
