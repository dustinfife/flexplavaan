    ### this script will simulate data that have inflated test statistics
    ### I'll use range enhancement, nonlinearity, large models, misspecification masked by measurement, etc. 
    ### missing factors


# load packages -----------------------------------------------------------

require(tidyverse)
require(flexplot)
require(lavaan)


# Define functions --------------------------------------------------------
logistic = function(x, max, slope, intercept){
  max/(1 + exp(-1*slope*(x-intercept)))
}
mme = function(x, max, midpoint){
  (max*x)/(midpoint + x)
}
trim = function(x,max, min){
  x[x>max] = max
  x[x<min] = min
  x
}
assignfactors = function(i, factor1, factor2, slopes, sd, switch=5){
  if (i<= switch){
    latent = factor1
  } else {
    latent = factor2
  }    
    unstand.slope = slopes[i]*(sd[i]/sd(latent))
    residual = (1-slopes[i]^2)*(sd[i]/sd(latent))
    x = unstand.slope*latent + rnorm(length(latent), 0, sqrt(residual))
  x
}
linearmodel = function(i, factor, slopes, sd){
  unstand.slope = slopes[i]*(sd(factor)/sd[i])
  residual = (1-slopes[i]^2)*(sd(factor)/sd[i])
  x = unstand.slope*factor + rnorm(length(factor), 0, sqrt(residual))
  x
}

# range enhancement -------------------------------------------------------
items = 5
latent = rbeta(round(runif(1, 300, 1000)), .005, .005)
slopes = seq(from=.015, to=.15, length.out=5)
intercept = seq(from=1, to=4, length.out=5)
enhancement = 1:5 %>% 
  map_dfc(~logistic(x=latent, max=5, slope=slopes[.x], intercept=runif(1,.1, 4))) %>% 
  mutate_all(function(x) x + rnorm(length(latent), 0, .033))
#enhancement$latent = latent
#pairs(enhancement)

# fit the model
model = "
f =~ V1 + V2 + V3 + V4 + V5
"
mod = cfa(model, data=enhancement)
summary(mod, fit.measures=TRUE)

# nonlinear relationships -------------------------------------------------
set.seed(1212)
items = 30
latent = rbeta(round(runif(1, 900, 1000)), 1, 5)
slopes = seq(from=.01, to=.2, length.out=items)
nonlinear = 1:items %>% 
  map_dfc(~mme(x=latent, max=5, midpoint=slopes[.x])) %>% 
  mutate_all(function(x) x + rnorm(length(latent), 0, .25 + x*.1)) %>% 
  mutate_all(trim, max=6, min=0) %>% 
  as.data.frame
nonlinear$latent = latent

# fit the model
model = paste0("f = ~ ", paste0("V", 1:items, collapse=" + "))
mod = cfa(model, data=nonlinear)
fitMeasures(mod, "rmsea")
standardizedsolution(mod)
#visualize(mod, subset=1:5)


# missing factor ----------------------------------------------------------
set.seed(75)
items = 6
a = rnorm(round(runif(1, 500, 800)))
b = .5*a + rnorm(length(a), 0, sqrt(1-.4^2))
sl = runif(items, .5, .9)
sds = runif(items, 1, 10)
missingfactor = 1:items %>% map_dfc(~assignfactors(.x, a, b, sl, sds, switch = 3)) %>% as.data.frame
model = paste0("f = ~ ", paste0("V", 1:items, collapse=" + "))
model2 = paste0("f1 = ~ ", paste0("V", 1:(items/2), collapse=" + "), 
                "\nf2 = ~ ", paste0("V", (items/2 + 1):items, collapse=" + "))
mod1 = cfa(model, data=missingfactor)
fitMeasures(mod1, "rmsea")
mod2 = cfa(model2, data=missingfactor)
fitMeasures(mod2, "rmsea")


# residual covariance -----------------------------------------------------
set.seed(75)
items = 6
a = rnorm(round(runif(1, 500, 800)))
b = .5*a + rnorm(length(a), 0, sqrt(1-.5^2))
sl = runif(items, .5, .9); sl[1] = .4
sds = runif(items, 1, 10)
residualcov = 1:items %>% map_dfc(~assignfactors(.x, a, b, sl, sds, switch = items/2)) %>% as.data.frame
residualcov$V2 = .2*(sd(residualcov$V2)/sd(residualcov$V6))*residualcov$V6 + residualcov$V2
cor(residualcov)
model1 = paste0("f1 = ~ ", paste0("V", 1:(items/2), collapse=" + "), 
                "\nf2 = ~ ", paste0("V", (items/2 + 1):items, collapse=" + "))
model2 = paste0(model1, "\nV2 ~~ V6")
mod1 = cfa(model1, data=residualcov)
mod2 = cfa(model2, data=residualcov)

fitMeasures(mod1, "rmsea")
fitMeasures(mod2, "rmsea")
visualize(mod1, mod2, method="lm")
viz_diagnostics(residualcov, aes(V2, V1), fit.lavaan = mod1, fit.lavaan2 = mod2)
summary(mod2, standardized=TRUE)
