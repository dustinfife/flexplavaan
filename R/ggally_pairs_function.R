d = read.csv("data/exp_data.csv")
#d = exercise_data
#p = ggplot(d, aes(x1, y3b))
#alpha = .2
data = d

  ## how to residualize item based on latent variable?
fit.lavaan <- readRDS("data/fit_lavaan.rds")

my_bin <- function(data, mapping, fit.lavaan, alpha=.5, ...) {
  
  ### plot ggplot object so I can extract the elements
  p = ggplot(data=data, mapping=mapping)
  
  ### extract name of variable in aes
  y = p$labels$y
  x = p$labels$x
  
  ### extract name of latent variables
  observed = lavNames(fit.lavaan)
  latent.names = lavNames(fit.lavaan, type="lv")
  
  ### extract lavaan objects
  #observed.pred = lavPredict(fit.lavaan, type="ov", method="Bartlett")
  latent.pred = lavPredict(fit.lavaan, type="lv", method="Bartlett")
  
  ### find index for the variable of interest
  #y.location = which(dimnames(observed.pred)[[2]]==y)
  #latent.names = dimnames(latent.pred)[[2]]
    
  ### predict observed from ALL latent (because otherwise relationships between x1 and y1, e.g., will be off)
  #loadings = attr(attr(fit.lavaan, "Model"),"GLIST")$lambda ## factor loading matrix
  #latent.variable.for.y = loadings[y.location,]!=0
  
  ### latent variables need to be in dataframe for ggplot
  data[,latent.names] = latent.pred
  
  ### this equation will predict observed from latent
  f = make.formula(y, latent.names)
  print(f)
  mod.fitted = lm(f, data=data)
  residual_name = paste0(y, "_resid")
  data[,residual_name] = residuals(mod.fitted)
  
  ### now add to ggplot object
  flexplot_form = make.formula(residual_name, x)
  print(flexplot_form)
  flexplot(flexplot_form, data=data, alpha=alpha, method="lm") + labs(y=paste0(y, " | latent"))
}
#head(data)

require(GGally); require(lavaan); require(flexplot)
observed = lavNames(fit.lavaan)
#ggpairs(d[,observed], upper = list(continuous = wrap(my_bin,fit.lavaan = fit.lavaan, alpha = .2)))
my_bin(data, aes(x1, x2), fit.lavaan)

head(data)
data[,c("latent_a_estimated", "latent_b_estimated")] = lavPredict(fit.lavaan, type="lv", method="Bartlett")
data$resids = residuals(lm(x2~A, data=data))
flexplot(resids~x1, data=data, method="lm")
plo
fit.lavaan$call
head(data)










flexplot(latent_b_estimated~latent_b, data=data)
### plot the relationship between x1 and x2


## residualize X2 based on latent variable
d$x2_residual = residuals(lm(x2~latent_a, data=d))
flexplot(x2_residual~x1, data=d)







#