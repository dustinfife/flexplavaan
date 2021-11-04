# this function sorts the DATA according to how the user specifies (for scatterplot matrix)
sort_dataset = function(object, sort_plots) {
  variable_order = sort_variables(object, sort_plots)
  d = get_lav_data(object) %>% dplyr::select(all_of(variable_order))
  return(d)
}

# this function sorts the *observed vector* according to how the user specifies (for scatterplot matrix)
sort_vector = function(object, sort_plots) {
  observed = lavNames(object, type="ov")
  variable_order = sort_variables(object, sort_plots, observed)
  d = get_lav_data(object) %>% dplyr::select(all_of(variable_order))
  return(d)
}

get_lav_data = function(object) {
  observed = lavNames(object)
  d = data.frame(lavInspect(object, "data"))
  names(d) = observed
  return(d)
}

get_subset = function(varnames, subset) {
  if (is.null(subset)) return(varnames)
  if (is.numeric(subset) & any(subset>max(length(varnames)))) stop("You're trying to index a varname using a number larger than the length of varname")
  if (!all(subset %in% varnames) & !is.numeric(subset)) stop("One or more of the variables you supplied in subset is not in varnames.")
  if (is.numeric(subset)) return(varnames[subset])
  return(varnames[varnames %in% subset])
}

sort_variables = function(object, sort_plots) {
  
  vector_of_booleans = rep(sort_plots, times=length(lavNames(object)))
  variable_order = ifelse(vector_of_booleans, 
                          block_model_residuals(object), 
                          1:length(lavNames(object)))
  return(variable_order)
}

flexplavaan_to_lavaan = function(fitted) {
  if (is.null(fitted)) return(NULL)
  if (class(fitted)=="flexplavaan") return(fitted$lavaan)
  return(fitted)
}
