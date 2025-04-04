set.seed(1212)
require(lavaan)
require(blavaan)
require(flexplot)
require(ggplot2)
require(tidyverse)

  #### simulate "latent" factor
factor = rnorm(1000, 5)

  ### specify loadings
loadings = c(.5, .6, .7)
nonlin.param = 4
linear = F  # change this to T to see what happens when relationships are linear

  #### simulate item scores
x1 = loadings[1]*factor + rnorm(length(factor), 0, sqrt(1-loadings[1]^2))
x2 = loadings[2]*factor + rnorm(length(factor), 0, sqrt(1-loadings[2]^2))
if (linear) {
  x3 = loadings[3]*factor + rnorm(length(factor), 0, sqrt(1-loadings[2]^2))
} else {
  x3 = nonlin.param^factor
}

  ### put into data frame
d = data.frame(x1=x1, x2=x2, x3=x3, f_real = factor)

  ### estimate fit with lavaan
model = '
  f =~ NA*x1 + x2 + x3
  f ~~ 1*f
'
fit = cfa(model, data=d)
fit.bayes = bcfa(model, data=d, mcmcfile=T, target="jags")

  ### create function to extract model-implied slope/intercept
model = nls(x3 ~ I((a^x2)*(b*x2)),
    data = d,
    start = list(a=4, b=.5))
f = function(x){
  a^x*(b*x)
}
a = coef(model)[1]; b = coef(model)[2]
plot(x2, x3)
ft = seq(min(x2), max(x2), length.out=100)
lines(ft, f(ft))

model_implied = function(fit, v1=2, v2=3){
  #if (linear){
    cov.mat = fitted(fit)$cov
    cor.mat = cov2cor(cov.mat)
    sd.vector = sqrt(diag(cov.mat))
    
    ### figure out model-implied coefficients
    b1 = cor.mat[v1,v2]*(sd.vector[v1]/sd.vector[v2])  # model implied
    b0 = mean(d[,v1]) - b1*mean(d[,v2]) 
    return(list(b1=b1, b0=b0))
 # } else {
    
  #}
}

coefs.frequentist = model_implied(fit)
coefs.bayesian = model_implied(fit.bayes)
coefs.regression = coef(lm(x2~x3, d))  # regression-estimated

### extract the factor scores
d$f_predicted = predict(fit)
d$f_predicted_bayes = predict(fit.bayes)


  ### plot pairwise relationships between variables
flexplot(x2~x3, data=d, se=F, suppress_smooth = T, alpha=.1) + 
  geom_abline(slope=coefs.frequentist$b1, intercept=coefs.frequentist$b0, col="red") +
  #geom_abline(slope=coefs.bayesian$b1, intercept=coefs.bayesian$b0, col="green") + 
  geom_abline(slope=coefs.regression[2], intercept=coefs.regression[1], col="blue") + 
  geom_smooth(col="black") 
  ## all methods (including bayesian) poorly estimate the relationship between items

  ### convert "real" and predicted estimates to factors for easier plotting
d = d %>% 
    mutate(real = scale(f_real), predicted=scale(f_predicted), predicted.bayes=f_predicted_bayes) %>%
    gather(method, factor.score, c(real, predicted, predicted.bayes)) 

  ### show how the two prediction methods differ
flexplot(x3~factor.score + method, data=d, suppress_smooth = F, alpha=.2) 
  ### both standard sem and bayes sem overestimate the values of the factor scores, though
  ### bayesian sem is less biased