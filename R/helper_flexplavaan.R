#' Fit a flexplavaan object
#' 
#' For efficiency, I recommend against fitting lavaan objects and instead fit
#' flexplavaan objects. These use identical syntax to lavaan, but also computes
#' standard errors. Computing standard errors is computationally intensive, so this
#' function inserts the standard errors within a flexplavaan class so it can be reused
#' (without being recomputed) later. 
#' @param model The model specified using lavaan syntax. 
#' @param data The dataset supplied by the user. 
#' @param ... Other arguments passed to lavaan. 
#'
#' @return A flexplavaan object with the following objects: lavaan, data, and standard_errors
#' @export
flexplavaan = function(model = NULL, data = NULL, ...) {
  
  # check for errors
  flexplavaan_check_errors(model, data)
  
  # fit lavaan model
  fitted = sem(model, data, ...)
  
  se_data = get_standard_errors(fitted)
  
  structure(list(lavaan = fitted, data = data, standard_errors = se_data),
            class = "flexplavaan")
}

flexplavaan_check_errors = function(model, data=NULL) {
  if (is.null(data)) stop("Flexplavaan requires raw data. Please provide a dataset.")
  return(NULL)
}

check_for_standard_errors = function(fitted) {
  
  if (class(fitted)=="flexplavaan") return(fitted$standard_errors)
  # message("It looks like you're visualizing lavaan objects. We recommend fitting a flexplavaan object\n
  #         instead to avoid having to recompute standard errors everytime. Type ?flexplavaan for more information.")
  return(get_standard_errors(fitted))
}

get_standard_errors = function(fitted) {

  # get names of variables
  latent_names = lavaan::lavNames(fitted, type="lv")
  latent_predicted = data.frame(lavPredict(fitted))
  
  # estimate standard errors
  #cat("Estimating Standard Errors...\n")
  se_data = 1:length(latent_names) %>% 
    purrr::map(~estimate_standard_errors(.x,fitted)$sd_imp) %>%  # returns list of se for each latent var
    data.frame
  names(se_data) = paste0("se_", latent_names)
  return(se_data)
}