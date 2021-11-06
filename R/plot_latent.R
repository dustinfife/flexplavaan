#fitted = fit_twofactor
# latent_plot(fit_bollen, formula = Eta2 ~ Eta1)
# latent_plot(fit_bollen)
latent_plot = function(fitted, fitted2 = NULL, estimate_se=T, method="loess", ...) {

  
  fitted_l = flexplavaan_to_lavaan(fitted)
  fitted2_l = flexplavaan_to_lavaan(fitted2)
  
  if (length(get_endogenous_names(fitted_l))<2) stop("You cannot do a latent plot when there's less than two endogenous variables.")
  
  latent_names = lavaan::lavNames(fitted_l, type="lv")
  latent_predicted = data.frame(lavPredict(fitted_l))

  # compute standard errors 
  se_data = check_for_sd_true(estimate_se, fitted, latent_names)
  
  model_names = get_and_check_names(fitted, fitted2)

  # add second dataset
  if (!is.null(fitted2_l)) {
    # get model names
    m1_name = model_names[1]
    m2_name = model_names[2]
    
    # get latent variable estimates and combine
    latent_predicted2 = data.frame(lavPredict(fitted2_l)) %>% 
      mutate(model = m2_name)
    latent_predicted = latent_predicted %>% 
      mutate(model = m1_name)
    latent_predicted = full_join(latent_predicted, latent_predicted2[,c("model",latent_names)], by=c("model", latent_names))
    
    # get sd for second dataset then combine
    se_data_2 = check_for_sd_true(estimate_se, fitted2, latent_names)[,names(se_data)]
    names(se_data_2) = names(se_data)
    se_data_2$model = m2_name
    se_data$model = m1_name
    se_data = full_join(se_data, se_data_2, by=names(se_data))
  }

  ### give them a formula (can be changed with modify_formula)
  formula = beta_to_flexplot(fitted_l, latent_predicted)
  if (formula[[1]]=="~") return(latent_plot_only(formula, latent_predicted, se_data, fitted_l, ...))
  
  # only return one plot
  return(latent_plot_only(formula[[1]], latent_predicted, se_data, fitted_l, ...))
}


check_for_sd_true = function(estimate_se, fitted, latent_names) {
  ### estimate standard errors
  if (estimate_se) return(check_for_standard_errors(fitted))
  
  se_data = data.frame(
    matrix(0, ncol=length(latent_names), nrow=nrow(fitted@Data@X %>% data.frame)))
  names(se_data) = paste0("se_", latent_names)
  return(se_data)
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
  data = create_ci_limits(data, f)
  
  ## see if alpha is set
  list(...)
  alpha_default = return_alpha(...)

  # get fit implied by the model
  if ("model" %in% names(data)) {
    f_vars = all.vars(f)
    dv = f_vars[1]; iv = f_vars[-1]
    f = flexplot:::make_flexplot_formula(c(iv, "model"), dv, data, ...)
  }

  plot_crosshair_plot(data, f, alpha_default)
}

create_ci_limits = function(data, formula) {
  xvar = all.vars(formula)[-1]
  yvar = all.vars(formula)[1]

  ### create limits of CI
  data[["lower_pi_xvar"]] = data[[xvar[1]]] - data[[paste0("se_", xvar[1])]]
  data[["lower_pi_yvar"]] = data[[yvar[1]]] - data[[paste0("se_", yvar[1])]]
  data[["upper_pi_xvar"]] = data[[xvar[1]]] + data[[paste0("se_", xvar[1])]]
  data[["upper_pi_yvar"]] = data[[yvar[1]]] + data[[paste0("se_", yvar[1])]]
  return(data)
}

plot_crosshair_plot = function(data, f, alpha_default, ...) {
  p = flexplot(f, data, se=F, ghost.line="red", sample=0, ...) + 
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
