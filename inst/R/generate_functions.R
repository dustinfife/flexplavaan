generate_latent = function(vars, loading, n, latent=NULL) {
  if (length(loading) ==1 ) loading = rep(loading, times=vars)

  # simulate latent
  if (is.null(latent)) latent = rnorm(n)
  
  # simulate loadings
  data = loading %>% map_dfc(function(x) x*latent + rnorm(n, 0, sqrt(1-x^2)))
  names(data) = paste0("x", 1:vars)
  return(data)
}