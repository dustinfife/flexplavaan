# d = read.csv("data/exp_data.csv")
# #d = exercise_data
# #p = ggplot(d, aes(x1, x2))
# #alpha = .2
# data = d
# 
#   ## how to residualize item based on latent variable?
# fit.lavaan <- readRDS("data/fit_lavaan.rds")
fit.lavaan = fitted
my_bin <- function(data, mapping, fit.lavaan, alpha=.5, residuals=FALSE, ...) {
  
  ### plot ggplot object so I can extract the elements
  p = ggplot(data=data, mapping=mapping)
  
  ### extract name of variable in aes
  y = p$labels$y
  x = p$labels$x
  
  ### extract name of latent variables
  observed = lavNames(fit.lavaan)
  latent.names = lavNames(fit.lavaan, type="lv")
  
  ### make a sequence for the x axis
  new.x = data.frame(x = seq(from=min(data[,x]), to=max(data[,x]), length.out=10))
  names(new.x) = x
  
  ### find index for the variable of interest
  y.location = which(observed==y)
  
  ## extract loadings
  loadings = inspect(fitted,what="std")$lambda ## factor loading matrix
  b = loadings[y,]
  
  ### estimate the fit (for the observed and new data)
  observed.pred = b^2*data[,x]
  new.x[,y] = b^2*new.x
  
  
  residual_name = paste0(y, "_resid")
  data[,residual_name] = data[,y] - observed.pred
  
  ### now add to ggplot object
  if (!residuals){
    flexplot_form = make.formula(y, x)
    flexplot(flexplot_form, data=data, alpha=alpha, method="loess") + geom_line(data=new.x, aes_string(x,y), col="red")
  } else {
    flexplot_form = make.formula(residual_name, x)
    flexplot(flexplot_form, data=data, alpha=alpha, method="loess") + labs(y=paste0(y, " | latent")) + geom_hline(yintercept=0, col="red")
  }
}



# 
# require(GGally); require(lavaan); require(flexplot)
# observed = lavNames(fit.lavaan)
# ggpairs(d[,observed], 
#         lower = list(continuous = wrap(my_bin,fit.lavaan = fit.lavaan, alpha = .2)),
#         upper = list(continuous = wrap(my_bin,fit.lavaan = fit.lavaan, alpha = .2, residuals=TRUE)),
#         )
# 
# 
# head(data)
# data[,c("latent_a_estimated", "latent_b_estimated")] = lavPredict(fit.lavaan, type="lv", method="Bartlett")
# data$resids = residuals(lm(x2~A, data=data))
# flexplot(resids~x1, data=data, method="lm")
# plo
# fit.lavaan$call
# head(data)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# flexplot(latent_b_estimated~latent_b, data=data)
# ### plot the relationship between x1 and x2
# 
# 
# ## residualize X2 based on latent variable
# d$x2_residual = residuals(lm(x2~latent_a, data=d))
# flexplot(x2_residual~x1, data=d)
# 
# 
# 
# 
# 
# 
# 
# #