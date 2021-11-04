#plot_scatter_matrix(fit_bollen$lavaan, subset=1:3)
#plot_scatter_matrix(fit_twofactor$lavaan, fit_twofactor_2$lavaan, subset=c("x1", "x2", "x3"), model_names=c("a", "b"))
# get rid of model_names argument by making it an attribute??
plot_scatter_matrix = function(object1, object2=NULL, subset=NULL, model_names=NULL, plot="model", ...) {
  
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
  
  ## set the class
  x = set_model_class(object1_l=object1_l, object2_l=object2_l, names=names, legend=legend, d=d, nms=nms, plot=plot)
  matrix_plot(x, ...)

}

matrix_plot = function(x, ...) {
  UseMethod("matrix_plot")
}

matrix_plot.all = function(x, ...) {
  p = with(x, ggpairs(d, legend=legend,
              lower = list(continuous = wrap(viz_diagnostics,fit.lavaan = object1_l, fit.lavaan2 = object2_l, alpha = .2, plot="disturbance", label_names=nms, invert.map=TRUE)),
              upper = list(continuous = wrap(viz_diagnostics,fit.lavaan = object1_l, fit.lavaan2 = object2_l, alpha = .2, plot="trace",       label_names=nms)),
              diag  = list(continuous = wrap(viz_diagnostics,fit.lavaan = object1_l, fit.lavaan2 = object2_l, alpha = .2, plot="histogram",   label_names=nms)))
           )
  if (!is.null(x$object2_l)) return(p)
  return(p + labs(title="Trail/DDP Plots", subtitle="Red=Implied, Blue=Observed"))
}

matrix_plot.trail = function(x, ...) {
  p = with(x, ggpairs(d, legend=legend,
              lower = NULL,
              upper = list(continuous = wrap(viz_diagnostics,fit.lavaan = object1_l, fit.lavaan2 = object2_l, alpha = .2, plot="trace",       label_names=nms)),
              diag  = list(continuous = wrap(viz_diagnostics,fit.lavaan = object1_l, fit.lavaan2 = object2_l, alpha = .2, plot="histogram",   label_names=nms)))
  )
  if (!is.null(x$object2_l)) return(p)
  return(p + labs(title="Trail/DDP Plots", subtitle="Red=Implied, Blue=Observed"))
}

matrix_plot.ddp = function(x, ...) {

  p = with(x, ggpairs(d, legend = rev(c(1,2)),
              lower = list(continuous = wrap(viz_diagnostics,fit.lavaan = object1_l, fit.lavaan2 = object2_l, alpha = .2, plot="disturbance", label_names=nms, invert.map=TRUE)),
              upper = NULL,
              diag  = list(continuous = wrap(viz_diagnostics,fit.lavaan = object1_l, fit.lavaan2 = object2_l, alpha = .2, plot="histogram",   label_names=nms)))
  )

  if (!is.null(x$object2_l)) return(p)
  return(p + labs(title="Trail/DDP Plots", subtitle="Red=Implied, Blue=Observed"))
}
