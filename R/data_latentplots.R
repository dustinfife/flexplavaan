check_for_sd_true = function(estimate_se, fitted, latent_names) {
  ### estimate standard errors
  if (estimate_se) return(check_for_standard_errors(fitted))
  
  se_data = data.frame(
    matrix(0, ncol=length(latent_names), nrow=nrow(fitted@Data@X %>% data.frame)))
  names(se_data) = paste0("se_", latent_names)
  return(se_data)
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