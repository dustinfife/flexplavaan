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
    plot_list = formula %>% purrr::map(~latent_plot_only(.x, latent_predicted, se_data, fitted))
    return(plot_list)
  }
  
  plot = latent_plot_only(formula, latent_predicted, se_data, fitted)
  return(plot)

}

# dv = dvs[1]
# i=1
# formula = forms
# data=latent_predicted
latent_plot_only = function(f, data, se_data, fitted) {

  xvar = all.vars(f)[-1]
  yvar = all.vars(f)[1]
  data = check_data_has_observed(cbind(data, se_data), xvar, yvar, fitted)
  browser()
  slope = compute_slope_angle(f, data)

  p = flexplot(f, data, se=F, ghost.line="red", alpha=0) + 
    ggforce::geom_ellipse(aes_string(x0 = xvar[1], y0 = yvar, a = paste0("se_", xvar[1]), b = paste0("se_", yvar), angle=slope), color=rgb(0,0,0,.2))
  return(p)
}
# 
# 
# # get list of 
# y_hat = lavPredict(fit_bollen)
# pooled_sd = estimate_standard_errors(1, fit_bollen)  # averaged stdev
# ?plausibleValues
# u = 
#   
#   
#   se_posterior = semTools::plausibleValues(fit_bollen)
# f
# fitted = fit_bollen
# aggregate_se = function(fitted) {
#   latent_name = lavaan::lavNames(fitted, type="lv") 
#   se_posterior = semTools::plausibleValues(fitted)
#   se_posterior = se_posterior %>% tibble::tibble() %>% unnest(cols=c(.)) %>%
#     group_by(case.idx) %>%
#     dplyr::summarize(within_sd = sd(!!sym(latent_name))) %>% 
#     data.frame()
#   se_posterior
# }


compute_slope_angle = function(f, data) {
  lm_model = lm(f, data)
  slope = lm(f, data) %>% standardized.beta*57.2958
  y_se = lm_model %>% predict %>% sd
  x_se = summary(lm_model)$sigma
  return(mod_slope[2])
}


# if an observed variable is endogenous, we need to return the full dataset
check_data_has_observed = function(data, xvar, yvar, fitted) {

  all_names = c(xvar, yvar)
  which_not_in_data = which(!(all_names %in% names(data)))
  varname_which = all_names[which_not_in_data]
  if (length(which_not_in_data)==0) return(data)

  
  full_data = cbind(data, data.frame(fitted@Data@X[[1]]))
  names(full_data) = c(names(data), fitted@Data@ov$name)
  se_name = paste0("se_", varname_which)
  full_data[[se_name]] = 0
  ## create column with standard deviation for the variable not in there
  
  return(full_data)
  
}
#fitted = fit_bollen
#beta_to_flexplot(fit_bollen, data.frame(lavPredict(fit_bollen)))
beta_to_flexplot = function(fitted, data, return_dvs=FALSE) {
  
  # beta matrix isn't just latent variables
  # if observed are endogenous, they will be there too
  # so I can't just use lavNames to get that
  end_names = fitted@Model@dimNames[[4]][[1]]
  
  # get the beta matrix (which is the path coefficients between latent variables)
  beta_matrix = fitted@Model@GLIST$beta

  # dvs will identify which endogenous variables have predictors
  dvs = which(rowSums(beta_matrix)>0)
  #end_names[get_dv_iv(dvs, beta_matrix)]
  beta_matrix
  
  if(return_dvs) return(dvs)
  model_formulas = dvs %>% purrr::map(~
                       flexplot:::make_flexplot_formula(
                         predictors = end_names[get_dv_iv(.x, beta_matrix)],
                         outcome = end_names[.x], 
                         data=data
                       )
                    )
  
  return(model_formulas)
}


#get_dv_iv(2, beta_matrix)
get_dv_iv = function(i, beta_matrix){
  which(abs(beta_matrix[i,])>0)
}
