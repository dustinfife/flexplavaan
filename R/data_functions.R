# this function sorts the DATA according to how the user specifies (for scatterplot matrix)
sort_dataset = function(object, sort_plots) {
  variable_order = sort_variables(object, sort_plots)
  d = get_lav_data(object) %>% dplyr::select(all_of(variable_order))
  return(d)
}

# this function sorts the *observed vector* according to how the user specifies (for scatterplot matrix)
sort_vector = function(object, sort_plots) {
  observed = lavNames(object, type="ov")
  variable_order = sort_variables(object, sort_plots)
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

get_all_data = function(fitted) {
  
  # get names of variables
  obs_names = lavaan::lavNames(fitted)
  latent_names = lavaan::lavNames(fitted, type="lv")
  
  # get dataset of observed
  obs_data = data.frame(fitted@Data@X)
  
  # get latent scores
  latent_raw = data.frame(lavaan::lavPredict(fitted))
  obs_data = data.frame(cbind(obs_data, latent_raw))
  names(obs_data) = c(obs_names, latent_names)
  return(obs_data)
}

#return a vector of residuals from predicting observed from latent
residual_from_latents = function(i, fitted) {
  
  # get names
  observed = lavNames(fitted)[i]
  variable_name = find_latents_for_observed(i, fitted)
  
  # if there are no latens associated with this variable, just return the variable
  if (is.na(variable_name[1])) return(fitted@Data@X[[1]][,i])
  # create dataset
  variable_scores = lavPredict(fitted, type = "lv")[,variable_name]
  dataset = data.frame(cbind(variable_scores, fitted@Data@X[[1]][,i]))
  names(dataset) = c(variable_name, observed)
  
  # formula/residuals
  formula_residual = flexplot::make.formula(observed, variable_name)
  residuals = residuals(lm(formula_residual, dataset))  
  return(residuals)
}

flexplavaan_to_lavaan = function(fitted) {
  if (is.null(fitted)) return(NULL)
  if (class(fitted)=="flexplavaan") return(fitted$lavaan)
  return(fitted)
}

#' Extract latent variables from a JAGs model
#'
#' @param jagsmod a runjags JAGs model
#'
#' @return the factor scores for the latent variables
#' @export
export_jags_latents = function(jagsmod){
  sum.jags = jagsmod
  lvs = startsWith(dimnames(sum.jags)[[1]], "eta")
  lvs = data.frame(sum.jags[lvs,"Mean"] ) %>% setNames("factor_score") 
  lvs$factor = dimnames(lvs)[[1]] %>% subsetString(",", 2) %>% gsub("]", "", .)
  lvs$id = dimnames(lvs)[[1]] %>% subsetString(",", 1) %>% gsub("eta[", "", ., fixed=T) #%>% mutate(id=as.numeric(id))
  lvs = lvs %>% spread(factor, factor_score) %>% setNames(c("id", paste0("factor", 1:(ncol(.)-1)))) %>% arrange(as.numeric(id))
  as.data.frame(lvs)
}

