#fitted = fit_twofactor
# latent_plot(fit_bollen, formula = Eta2 ~ Eta1)
# latent_plot(fit_bollen)
latent_plot = function(fitted, formula = NULL, estimate_se=T,...) {
  
  latent_names = lavaan::lavNames(fitted, type="lv")
  latent_predicted = data.frame(lavPredict(fitted))

  ### estimate standard errors
  se_data = check_for_standard_errors(fitted)
  
  ### get flexplot formulae
  if (is.null(formula)) { 
    formula = beta_to_flexplot(fitted, latent_predicted)
    if (formula[[1]]=="~") return(latent_plot_only(formula, latent_predicted, se_data, fitted, ...))
    plot_list = formula %>% purrr::map(~latent_plot_only(.x, latent_predicted, se_data, fitted, ...))
    return(plot_list)
  }
  
  plot = latent_plot_only(formula, latent_predicted, se_data, fitted, ...)
  return(plot)

}


# dv = dvs[1]
# i=1
# formula = forms
# data=latent_predicted
latent_plot_only = function(f, data, se_data, fitted, ...) {

  xvar = all.vars(f)[-1]
  yvar = all.vars(f)[1]
  data = check_data_has_observed(cbind(data, se_data), xvar, yvar, fitted)

  ### create limits of CI
  data[["lower_pi_xvar"]] = data[[xvar[1]]] - data[[paste0("se_", xvar[1])]]
  data[["lower_pi_yvar"]] = data[[yvar[1]]] - data[[paste0("se_", yvar[1])]]
  data[["upper_pi_xvar"]] = data[[xvar[1]]] + data[[paste0("se_", xvar[1])]]
  data[["upper_pi_yvar"]] = data[[yvar[1]]] + data[[paste0("se_", yvar[1])]]
  
  ## see if alpha is set
  alpha_default = return_alpha(...)

  #browser()
  p = flexplot(f, data, se=F, ghost.line="red", alpha=0) + 
    geom_point() +
    alpha_default[1] +
    alpha_default[2]
  p = switch_layer_orders(p)
  return(p)
}


return_alpha = function(...) {

  if (is.null(names(list(...)))) {
    return(
      c(geom_errorbar(aes(ymin=lower_pi_yvar, ymax=upper_pi_yvar),alpha = .3),
       geom_errorbarh(aes(xmin=lower_pi_xvar, xmax=upper_pi_xvar),alpha = .3) ))
  } 
  if ("alpha" %in% names(list(...))) {
    return(
    c(geom_errorbar(aes(ymin=lower_pi_yvar, ymax=upper_pi_yvar),...),
      geom_errorbarh(aes(xmin=lower_pi_xvar, xmax=upper_pi_xvar), ...) )
    )
  } 
  
  
  return(c(geom_errorbar(aes(ymin=lower_pi_yvar, ymax=upper_pi_yvar),alpha = .3, ...),
      geom_errorbarh(aes(xmin=lower_pi_xvar, xmax=upper_pi_xvar),alpha = .3, ...) ))


}

switch_layer_orders = function(p){
  ## find the geom_smooth
  layers = sapply(p$layers, function(x) class(x$geom)[1])
  smooth_index = which(layers == "GeomSmooth")

  ## remove it, then put it back in the last position
  smooth_line = p$layers[[smooth_index]]
  p$layers[[smooth_index]] = NULL
  p$layers[[length(layers)]] = smooth_line
  return(p)
}

aggregate_se = function(fitted, ...) {
  latent_name = lavaan::lavNames(fitted, type="lv")
  pred_imputations = semTools::plausibleValues(fit_bollen)
  a = pred_imputations %>% 
    tibble::tibble() %>%                      # store as a nested tibble
    purrr::set_names(`.`,"imputation") %>%    # rename column from "." to "imputation"
    mutate(imp = 1:n()) %>%                   # create a new column that indexes the imputation
    unnest(cols=c(imputation, imp)) %>%       # puts data in long format
    group_by(case.idx) %>%                    # computes sd per perso
    summarize_all(sd)
  return(a)
}


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
  if (length(fitted@Model@dimNames)>3) {
    end_names = get_endogenous_names(fitted)
  
    # get the beta matrix (which is the path coefficients between latent variables)
    beta_matrix = fitted@Model@GLIST$beta

    # dvs will identify which endogenous variables have predictors
    dvs = which(rowSums(beta_matrix)>0)
    
    model_formulas = dvs %>% purrr::map(~
                                          flexplot:::make_flexplot_formula(
                                            predictors = end_names[get_dv_iv(.x, beta_matrix)],
                                            outcome = end_names[.x], 
                                            data=data
                                          )
    )
    if (return_dvs) return(dvs)
    model_formulas
    
  # if they don't have endogenous variables, just feed it to flexplot  
  } else {
    dvs = lavaan::lavNames(fitted, type="lv")
    if (return_dvs) return(1:length(dvs))
    model_formulas = flexplot:::make_flexplot_formula(predictors = dvs[-1],
                                                      outcome = dvs[1], 
                                                      data=data)
    return(model_formulas)
  }
}

get_endogenous_names = function(fitted){
  # lavaan matrices are lambda, theta, psi, and beta.
  # beta specifies which variables are endogenous.
  # if there's no beta, all relationships are correlational
  if (length(fitted@Model@dimNames)>3) {
    return(fitted@Model@dimNames[[4]][[1]])
  }
  return(fitted@Model@dimNames[[3]][[1]])
}


#get_dv_iv(2, beta_matrix)
get_dv_iv = function(i, beta_matrix){
  which(abs(beta_matrix[i,])>0)
}
