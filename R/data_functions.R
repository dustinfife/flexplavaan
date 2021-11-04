# this function sorts the DATA according to how the user specifies (for scatterplot matrix)
sort_dataset = function(object, sort_plots, plot) {
  variable_order = sort_variables(object, sort_plots, plot)
  d = get_lav_data(object) %>% dplyr::select(all_of(variable_order))
  return(d)
}

# this function sorts the *observed vector* according to how the user specifies (for scatterplot matrix)
sort_vector = function(object, sort_plots, plot) {
  observed = lavNames(object, type="ov")
  variable_order = sort_variables(object, sort_plots, plot, observed)
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

sort_variables = function(object, sort_plots, plot) {
  
  plot_type = plot %in% c("all", "disturbance", "model")
  repeated_condition = rep(sort_plots & plot_type, times=length(lavNames(object)))
  variable_order = ifelse(repeated_condition, 
                          block_model_residuals(object), 
                          1:length(lavNames(object)))
  return(variable_order)
}

flexplavaan_to_lavaan = function(fitted) {
  if (is.null(fitted)) return(NULL)
  if (class(fitted)=="flexplavaan") return(fitted$lavaan)
  return(fitted)
}