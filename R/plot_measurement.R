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


check_for_standard_errors = function(fitted) {
  if (class(fitted)=="flexplavaan") return(fitted$standard_errors)
  message("It looks like you're visualizing lavaan objects. We recommend fitting a flexplavaan object\n
          instead to avoid having to recompute standard errors everytime. Type ?flexplavaan for more information.")
  return(get_standard_errors(fitted))
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
apply_measurement_plot = function(i, fitted, ... ) {
 
  # get observed data and sample (if specified)
  data = create_latent_dataset(i, fitted)
  
  # get latent name
  latent = lavaan::lavNames(fitted, type="lv")[i]

  # append measurement
  se = estimate_standard_errors(i, fitted)
  data$se = rep(se$sd_imp, times=length(unique(data$Measure)))
  data$lower = data[,latent] - data$se
  data$upper = data[,latent] + data$se
  
  formula_p = as.formula(paste0(latent, "~Observed|Measure"))
  # suppress datapoints so I can get the fitted line for the actual dataset
  # then I can add dots as a separate geom based on sampling
  p = flexplot(formula_p,
           data=data,
           method="lm",
           ghost.line = "red", alpha=0,...) 
  d_sampled = random_sample_from_data(data, ...)

  p = p + 
    geom_point(data=d_sampled) +
    geom_errorbar(data=d_sampled, aes(ymin=lower, ymax=upper), alpha = .2)
  p = put_geom_last(p)
  # editing the geom_smooth because it keeps giving message saying "using formula y~x"
  # now I'm making that explicit (and it wouldn't work using suppress_smooth=T)
  p$layers[[length(p$layers)]] = geom_smooth(method="lm", formula = y~x)  
  p = put_geom_last(p, "GeomLine")

  return(p)
}


put_geom_last = function(p, name="GeomSmooth") {
  layer_names = sapply(p$layers, function(x) class(x$geom)[1])
  
  which_is_smooth = which(layer_names==name)
  if (length(which_is_smooth)<1) return(p)  ### if the geom doesn't exist, just return the plot
  smooth_layer = p$layers[[which_is_smooth[1]]]
  
  #nullify the smooth layer
  p$layers[[which_is_smooth]] = NULL
  p$layers = c(p$layers, smooth_layer)
  return(p)
}


#
random_sample_from_data = function(data, ...) {
  if (is.null(names(list(...)))) return(data)
  if ("sample" %in% names(list(...))) {
    options = list(...)
    return(data[sample(1:nrow(data), size=min(nrow(data), options$sample)), ])
  }
}



#' Produce a measurement plot for a lavaan object
#'
#' @param fitted a `lavaan` object 
#' @param latent_vars which latent variables should be plotted? Defaults to plot all of them.
#' @param ... Other arguments passed to flexplot. 
#'
#' @return This function returns either a single plot (if `length(latent_vars)==1`), or it will return a list of plots. 
#' @export
#' @importFrom purrr map
#'
#' @examples
#' measurement_plot(fit_bollen, 1)
#' measurement_plot(fit_bollen, 1:2)
measurement_plot = function(fitted, latent_vars=NULL, ...) {

  ## return the latent variable index (if necessary)
  fitted = flexplavaan_to_lavaan(fitted)
  latent_index = return_latent_index(fitted, latent_vars)
  
  if (length(latent_index)==1) {
    return(apply_measurement_plot(latent_index, fitted, ...))
  }

  plot_list = latent_index %>% purrr::map(function(.x, ...) { suppressMessages(apply_measurement_plot(.x, fitted, ...))}, ...)
  names(plot_list) = lavNames(fitted, type="lv")[latent_index]
  msg = paste0("There are ", length(latent_index), " measurement plots. I'm going to list the names of them below so you know how to access them. \n")
  message(msg)
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
