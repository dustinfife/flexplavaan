# this function sorts the DATA according to how the user specifies (for scatterplot matrix)
sort_dataset = function(object, sort_plots) {
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

# this returns both observed and latent variables
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


#' #' Extract latent variables from a JAGs model
#' #'
#' #' @param jagsmod a runjags JAGs model
#' #'
#' #' @return the factor scores for the latent variables
#' #' @export
#' export_jags_latents = function(jagsmod){
#'   sum.jags = jagsmod
#'   lvs = startsWith(dimnames(sum.jags)[[1]], "eta")
#'   lvs = data.frame(sum.jags[lvs,"Mean"] ) %>% setNames("factor_score") 
#'   lvs$factor = dimnames(lvs)[[1]] %>% subsetString(",", 2) %>% gsub("]", "", .)
#'   lvs$id = dimnames(lvs)[[1]] %>% subsetString(",", 1) %>% gsub("eta[", "", ., fixed=T) #%>% mutate(id=as.numeric(id))
#'   lvs = lvs %>% spread(factor, factor_score) %>% setNames(c("id", paste0("factor", 1:(ncol(.)-1)))) %>% arrange(as.numeric(id))
#'   as.data.frame(lvs)
#' }

