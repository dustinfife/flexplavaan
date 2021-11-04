#plot_scatter_matrix(fit_bollen$lavaan, subset=1:3)
#plot_scatter_matrix(fit_twofactor$lavaan, fit_twofactor_2$lavaan, subset=c("x1", "x2", "x3"), model_names=c("a", "b"))
plot_scatter_matrix = function(object1, object2=NULL, subset=NULL, model_names=NULL) {
  
  object1_l = flexplavaan_to_lavaan(object1)
  object2_l = flexplavaan_to_lavaan(object2)
 
  # specify subsets
  d = sort_dataset(object1_l, sort_plots = TRUE) %>% 
    dplyr::select(dplyr::all_of(get_subset(lavNames(object1_l),
                             subset)))

  # get legend
  legend = get_legend(object2_l)
  
  ## get names
  nms = get_and_check_names(model_names, object1_l, object2_l)
  
  p = ggpairs(d, legend=legend,
              lower = list(continuous = wrap(viz_diagnostics,fit.lavaan = object1_l, fit.lavaan2 = object2_l, alpha = .2, plot="disturbance", label_names=nms, invert.map=TRUE)),
              upper = list(continuous = wrap(viz_diagnostics,fit.lavaan = object1_l, fit.lavaan2 = object2_l, alpha = .2, plot="trace",       label_names=nms)),
              diag  = list(continuous = wrap(viz_diagnostics,fit.lavaan = object1_l, fit.lavaan2 = object2_l, alpha = .2, plot="histogram",   label_names=nms)))
  if (!is.null(object2_l)) return(p)
  return(p + labs(title="Trail/DDP Plots", subtitle="Red=Implied, Blue=Observed"))
  
}

# p = ggpairs(d, legend=legend,
#               lower = list(continuous = wrap(viz_diagnostics,fit.lavaan = object_l, fit.lavaan2 = object2_l, alpha = .2, plot="disturbance", label_names=nms, invert.map=TRUE,...)),
#               diag =  list(continuous = wrap(viz_diagnostics,fit.lavaan = object_l, fit.lavaan2 = object2_l, alpha = .2, plot="histogram",   label_names=nms, ...)),
#               upper = NULL)    
# if (is.null(object2_l)) {
#   p = p + labs(title="DD Plots", subtitle="Red=Implied, Blue=Observed")