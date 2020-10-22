# fitted = fit.lavaan
# 
# measurement_plot = function(fitted) {
#   
#   ## extract latent and observed data
#   latents = lavPredict(fitted)
#   observed = fitted@Data@X
#   
#   
# }

new_scale = function(x){
  x = scale(x)
  attr(x, "scaled:center") = NULL
  attr(x, "scaled:scale") = NULL
  x
}


#' @import dplyr
#' @import tidyr
#' @import lavaan
create_latent_dataset = function(i, fitted) {
  
  # get names of variables
  obs_names = lavaan::lavNames(fitted)
  latent_names = lavaan::lavNames(fitted, type="lv")
  
  # get dataset of observed
  obs_data = data.frame(fitted@Data@X)

  # get latent scores
  #browser()
  latent_raw = data.frame(lavaan::lavPredict(fitted)[,i])
  obs_data = data.frame(cbind(obs_data, latent_raw))
  names(obs_data) = c(obs_names, latent_names[i])

  ## remove columns not associated with this latent variable
  #
  lambda_factor_i = fitted@Model@GLIST$lambda[,i] %>% data.frame 
  indicators_for_factor_i = which(lambda_factor_i!=0)
  obs_data = obs_data[,c(obs_names[indicators_for_factor_i], latent_names[i])]
  obs_name_remaining = obs_names[indicators_for_factor_i]
  
  ## convert to long format
  long_measurement = obs_data %>% 
      dplyr::mutate_at(obs_name_remaining, new_scale)  %>% 
      gather(key="Measure", value="Observed", obs_name_remaining) %>% 
      select(Measure, Observed) 
  
  long_measurement[[latent_names[i]]] = as.numeric(unlist(rep(obs_data[,latent_names[i]], times=length(obs_name_remaining))))
  return(long_measurement)

}



#' @importFrom semTools plausibleValues
estimate_standard_errors = function(i,fitted) {
  latent_name = sym(lavaan::lavNames(fitted, type="lv")[i])
  se_posterior = semTools::plausibleValues(fitted)
  se_posterior = se_posterior %>% tibble::tibble() %>% unnest(cols=c(.)) %>%
    group_by(case.idx) %>%
    dplyr::summarize(sd_imp = sd(!!latent_name)) %>% 
    data.frame()
 se_posterior
}


#apply_measurement_plot(1, fit_twofactor)
# i = 1; fitted = fit_twofactor
apply_measurement_plot = function(i, fitted, ...) {

  # get observed data
  data = create_latent_dataset(i, fitted)
  
  # get latent name
  latent = lavaan::lavNames(fitted, type="lv")[i]
  
  # append measurement
  se = estimate_standard_errors(i, fitted)
  data$se = rep(se$sd_imp, times=length(unique(data$Measure)))
  data$lower = data[,latent] - data$se
  data$upper = data[,latent] + data$se
  # plot it
  formula_p = as.formula(paste0(latent, "~Observed|Measure"))

  p = flexplot(formula_p,
           data=data,
           method="lm",
           ghost.line = "red", alpha=.4, ...) +
    geom_errorbar(aes(ymin=lower, ymax=upper), alpha = .2)
  
  return(p)
}



#' Produce a measurement plot for a lavaan object
#'
#' @param fitted a `lavaan` object 
#' @param latent_vars which latent variables should be plotted? Defaults to plot all of them.
#'
#' @return This function returns either a single plot (if `length(latent_vars)==1`), or it will return a list of plots. 
#' @export
#' @importFrom purrr map
#'
#' @examples
#' measurement_plot(fitted, 1)
#' measurement_plot(fitted, 1:2)
measurement_plot = function(fitted, latent_vars=NULL) {
  
  ## return the latent variable index (if necessary)
  latent_index = return_latent_index(fitted, latent_vars)
  
  if (length(latent_index)==1) {
    return(apply_measurement_plot(latent_index, fitted))
  }
  

  plot_list = latent_vars %>% purrr::map(~apply_measurement_plot(.x, fitted))
  return(plot_list)
}



find_nth = function(n){
  if (n==1) return("st")
  if (n==2) return("nd")
  if (n==3) return("rd")
  return("th")
}



return_latent_index = function(fitted, latent_var = NULL) {
  all_latents = lavaan::lavNames(fitted, type="lv")
  
  if (is.null(latent_var)) return(1:length(all_latents))
  
  if (is.numeric(latent_var) & max(latent_var) > length(all_latents)) {
    msg = paste0("Whoa there! You're trying to plot the ", max(latent_var), find_nth(latent_var), " latent variable when there's only ", length(all_latents))
    stop(msg)
  }
  
  # if they supply a number, return it
  if (is.numeric(latent_var)) return(latent_var)

  # if they supply a character, make sure it's there
  if (!(any(latent_var %in% all_latents))) {
    bad_var = which(!(latent_var %in% all_latents))
    stop("Whoooooa, buddy! It looks like you're trying to visualize the variable '", latent_var[bad_var][1], "' when it's not a latent variable in your model.")
  }  
  
  return(which(all_latents %in% latent_var))

}