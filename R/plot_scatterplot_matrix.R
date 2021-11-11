#plot_scatter_matrix(fit_bollen$lavaan, subset=1:3)
#plot_scatter_matrix(fit_twofactor$lavaan, fit_twofactor_2$lavaan, subset=c("x1", "x2", "x3"), model_names=c("a", "b"))
# get rid of model_names argument by making it an attribute??
plot_scatter_matrix = function(object1, object2=NULL, subset=NULL, plot="all", ...) {
  
  object1_l = flexplavaan_to_lavaan(object1)
  object2_l = flexplavaan_to_lavaan(object2)
 
  # specify subsets
  names_ordered = lavNames(object1_l)[sort_variables(object1_l, T)][subset]
  d = sort_dataset(object1_l, sort_plots = TRUE) %>% 
    dplyr::select(dplyr::all_of(names_ordered))

  # get legend
  legend = get_legend(object2_l)
  
  ## get names

  nms = get_and_check_names(model_names=NULL, object1, object2)
  
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

  p = with(x, ggpairs(d, legend = legend,
              lower = list(continuous = wrap(viz_diagnostics,fit.lavaan = object1_l, fit.lavaan2 = object2_l, alpha = .2, plot="disturbance", label_names=nms, invert.map=TRUE)),
              upper = NULL,
              diag  = list(continuous = wrap(viz_diagnostics,fit.lavaan = object1_l, fit.lavaan2 = object2_l, alpha = .2, plot="histogram",   label_names=nms)))
  )

  if (!is.null(x$object2_l)) return(p)
  return(p + labs(title="Trail/DDP Plots", subtitle="Red=Implied, Blue=Observed"))
}

estimate_linear_fit = function(fit.lavaan, x, y, data){
  
  if (class(fit.lavaan)=="matrix"){
    data(nonlinear)
    data = nonlinear
    fit.lavaan = results
    x = "V3"; y = "V2"
    rel.x = var(nonlinear$V3)-fit.lavaan["theta[1,1,1]","Mean"]^2
    rel.y = 1- (fit.lavaan["theta[2,2,1]","Mean"]^2/var(nonlinear$V2))
    
    lvs = startsWith(dimnames(fit.lavaan)[[1]], "eta")
    lvs = fit.lavaan[lvs,"Mean"] #%>% as.data.frame 
    data$f2 = lvs
    
    flexplot::flexplot(f2~latent, data=data)
    
    newdata = cbind(data, lvs) %>% 
      data.frame %>% 
      mutate(!!"f" := rescale(f, mean(!!(as.name(y))), sd(!!(as.name(y)))))
    
    x_new = quantile(newdata[,x], probs = seq(from=0, to=1, length.out=80)) %>% as.numeric
    y_new = quantile(newdata[,"f"], probs = seq(from=0, to=1, length.out=80)) %>% as.numeric
    y_new = mean(y_new) + sqrt(rel.x*rel.y) * (y_new-mean(y_new))
    y_new = y_new - 3
    # transform f to reduce it's slope by reliability
    fnew = newdata[["f"]]
    newdata[["f"]] = mean(fnew) + sqrt(rel.x*rel.y) * (fnew-mean(fnew))    
    residuals = newdata[[y]] - newdata[["f"]]
    
    flexplot(V3~V2, data=data) +
      geom_line(data=data.frame(x_new=x_new, y_new=y_new), aes(x_new, y_new), col="red")
    
  }  
  
  
  
  if (class(fit.lavaan)=="blavaan"){
    
    lambda = inspect(fit.lavaan,what="std")$lambda
    rel.x = sum(lambda[x,])^2  ### DO YOU SUM FACTOR LOADINGS TO GET RELIABILITY?
    rel.y = sum(lambda[y,])^2
    factors = names(which(abs(lambda[y,])>0))
    
    lvs = data.frame(blavInspect(fit.lavaan, 'lvmeans')) %>% 
      setNames(dimnames(lambda)[[2]]) %>% 
      mutate(!!"f" := !!(as.name(factors))) %>%
      select("f")
    
    newdata = cbind(data, lvs) %>% 
      data.frame %>% 
      mutate(!!"f" := rescale(f, mean(!!(as.name(y))), sd(!!(as.name(y)))))
    
    x_new = quantile(newdata[,x], probs = seq(from=0, to=1, length.out=30)) %>% as.numeric
    y_new = quantile(newdata[,"f"], probs = seq(from=0, to=1, length.out=30)) %>% as.numeric
    y_new = mean(y_new) + sqrt(rel.x*rel.y) * (y_new-mean(y_new))
    
    # transform f to reduce it's slope by reliability
    fnew = newdata[["f"]]
    newdata[["f"]] = mean(fnew) + sqrt(rel.x*rel.y) * (fnew-mean(fnew))    
    residuals = newdata[[y]] - newdata[["f"]]
  } else {
    
    ### compute model-implied slope between the two
    implied.cor = lavInspect(fit.lavaan, what="cor.ov")
    implied.cov = lavInspect(fit.lavaan, what="cov.ov")
    stdev_ov = sqrt(diag(implied.cov))
    estimated.slope = implied.cor[x,y]*(stdev_ov[y]/stdev_ov[x])
    ### slope = sd(y)/sd(x) = maximum possible slope between the two, but multiply by f(reliability)
    corrected.slope = estimated.slope
    corrected.intercept = mean(data[,y]) - corrected.slope * mean(data[,x])
    ### maximum possible value * sqrt(reliability product)
    
    x_new = seq(from=min(data[,x]), to=max(data[,x]), length.out=20)
    y_new = corrected.intercept + corrected.slope*x_new
    residuals = data[,y] - (corrected.intercept + corrected.slope*data[,x])
  }
  
  list(x_new = x_new, y_new = y_new, residuals = residuals)
}

estimate_nonlinear_fit = function(latentscores, x, y, data, which.latent=c(1,1)){
  
  #browser()
  latent.names = unique(names(latentscores)[(which.latent+1)])
  newdata = cbind(data, latentscores) %>% 
    data.frame 
  
  ### use loess to fit (to estimate reliability)
  fit_y = flexplot::make.formula(y,latent.names[which.latent[2]]) %>% loess(newdata, degree=2)
  fit_x = flexplot::make.formula(x,latent.names[which.latent[1]]) %>% loess(newdata, degree=2)
  
  rel.y = var(fit_y$fitted-mean(newdata[,y]))/var(newdata[,y])
  rel.x = var(fit_x$fitted-mean(newdata[,x]))/var(newdata[,x])
  
  ### generate line for prediction (x is always the latent variable, because that's how it was generated)
  y_new = quantile(newdata[,y], probs = seq(from=0, to=1, length.out=80)) %>% as.numeric
  x_new = quantile(newdata[,"magic_skills"], probs = seq(from=0, to=1, length.out=80)) %>% as.numeric
  x_new = fifer::rescale(x_new, new.mean = mean(newdata[,x]), new.sd = sd(newdata[,x]))
  x_new2 = mean(x_new) + sqrt(rel.x*rel.y)* (x_new-mean(x_new))
  
  #### figure out the mean of the latent for every value of x, then transform
  quants = quantile(newdata[,x], seq(0,1,length.out=30))
  newdata$breaks = cut(newdata[,x], breaks=quants, include.lowest=T)
  predicted = newdata %>% group_by(breaks) %>% 
    summarize(factors = mean(!!(as.name(latent.names[1])))) %>% 
    mutate(breaks = round(quants)[-1]) %>% 
    mutate(factors = fifer::rescale(factors, mean((newdata[,y])), sd(newdata[,y]))) %>% 
    mutate(factors2 = mean(factors) + sqrt(rel.x*rel.y) * (factors-mean(factors)))
  
  form = flexplot::make.formula(y, x)
  flexplot(form, data=data) +
    geom_line(data=predicted, aes(breaks, factors), col="red") +
    geom_line(data=predicted, aes(breaks, factors2), col="blue") 
  
}
