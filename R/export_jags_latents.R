#jagsmod = hogwarts_nonlinear
export_jags_latents = function(jagsmod){
  sum.jags = jagsmod
  lvs = startsWith(dimnames(sum.jags)[[1]], "eta")
  lvs = data.frame(sum.jags[lvs,"Mean"] ) %>% setNames("factor_score") 
  lvs$factor = dimnames(lvs)[[1]] %>% subsetString(",", 2) %>% gsub("]", "", .)
  lvs$id = dimnames(lvs)[[1]] %>% subsetString(",", 1) %>% gsub("eta[", "", ., fixed=T) #%>% mutate(id=as.numeric(id))
  lvs = lvs %>% spread(factor, factor_score) %>% setNames(c("id", paste0("factor", 1:(ncol(.)-1)))) %>% arrange(as.numeric(id))
  as.data.frame(lvs)
}

