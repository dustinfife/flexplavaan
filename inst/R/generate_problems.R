require(lavaan)
require(tidyverse)
source("inst/R/generate_functions.R")


# correct model
x_latent = rnorm(5000)
y_latent = .1*x_latent + rnorm(length(x_latent), 0, sqrt(1-.1^2))
x_vars = generate_latent(vars=4, loading=runif(4, .6, .8), n=1000, x_latent) 
y_vars = generate_latent(vars=4, loading=runif(4, .6, .8), n=1000, y_latent)
names(y_vars) = paste0("y", 1:4)
d = cbind(x_vars, y_vars)
model = "
latent_x =~ x1 + x2 + x3 + x4 
latent_y =~ y1 + y2 + y3 + y4
latent_x~~latent_y
latent_x ~~ latent_x
latent_y ~~ latent_y
"
fit = cfa(model, d)
summary(fit, fit.measures=TRUE, standardized=TRUE)
implied_measurement(fit, "latent_y")


# variable that doesn't load on any factor (but it is correlated with others)
x_latent = rnorm(1000)
y_latent = .1*x_latent + rnorm(length(x_latent), 0, sqrt(1-.1^2))
x_vars = generate_latent(vars=4, loading=runif(4, .4, .7), n=1000, x_latent) 
y_vars = generate_latent(vars=4, loading=runif(4, .4, .7), n=1000, y_latent)
names(y_vars) = paste0("y", 1:4)
x_vars$x5 = with(x_vars, .6*x4 + rnorm(length(x_latent), 0, sqrt(1-.6^2)))
d = cbind(x_vars, y_vars)
model = "
latent_x =~ x1 + x2 + x3 + x4 + x5
latent_y =~ y1 + y2 + y3 + y4
latent_x~~latent_y
"
model2 = "
latent_x =~ x1 + x2 + x3 + x4
latent_y =~ y1 + y2 + y3 + y4
x5 ~ x4
latent_x~~latent_y
"
fit = cfa(model, d)
fit2 = cfa(model2, d)
summary(fit)
cbind(fitMeasures(fit), fitMeasures(fit2)) %>% round(digits=3)
summary(fit2)
visualize(fit, fit2, subset=c("x5", "x4", "x1"))
implied_measurement(fit, "latent_x")
  # interesting....the plot basically shows x4 and the latent are the same thing
  # but only for the misspecified model
implied_measurement(fit2, "latent_x")





# variable that loads on two factors
n = 800
x_latent = rnorm(n)
y_latent = .4*x_latent + rnorm(length(x_latent), 0, sqrt(1-.4^2))
x_vars = generate_latent(vars=4, loading=c(.3, .4, .5, .2), n=n, x_latent) 
y_vars = generate_latent(vars=4, loading=c(.3, .4, .5, .6), n=n, y_latent)
names(y_vars) = paste0("y", 1:4)
d = cbind(x_vars, y_vars)
d$y4 = with(d, y4 + .8*x_latent)
model = "
latent_x =~ x1 + x2 + x3 + x4
latent_y =~ y1 + y2 + y3 + y4
latent_y ~~ latent_x
"
fit = cfa(model, d)
implied_measurement(fit, "latent_x")



# missing association between latents
n = 1000
x_latent = rnorm(n)
z_latent = .5*x_latent + rnorm(length(x_latent), 0, sqrt(1-.3^2))
y_latent = .3*x_latent + .3*y_latent + rnorm(n, 0, .8)
x_vars = generate_latent(vars=4, loading=runif(4, .4, .7), n=n, x_latent) 
y_vars = generate_latent(vars=4, loading=runif(4, .4, .7), n=n, y_latent)
z_vars = generate_latent(vars=4, loading=runif(4, .4, .7), n=n, z_latent)
names(y_vars) = paste0("y", 1:4)
names(z_vars) = paste0("z", 1:4)
d = cbind(x_vars, y_vars, z_vars)
model = "
latent_x =~ x1 + x2 + x3 + x4
latent_y =~ y1 + y2 + y3 + y4
latent_z =~ z1 + z2 + z3 + z4
latent_y ~ latent_x + latent_z
latent_x ~~ 0*latent_z
"
fit = cfa(model, d, )
summary(fit, fit.measures=TRUE, standardized=TRUE)
implied_measurement(fit, "latent_x", limit=6)
implied_measurement(fit, "latent_z")
implied_measurement(fit, "latent_y")
visualize(fit, subset=1:5, method="lm")


summary(fit_bollen)
implied_measurement(fit_bollen, "Eta1")


set.seed(2323)
# model with uncorrelated factors
d = generate_latent(vars=4, loading=0, n=10000) 

model = "
latent_x =~ 0*x1 + x2 
latent_y =~ x3 + x4 
latent_x ~~ 0*latent_y
"
noloadings = cfa(model, d)
summary(fit, fit.measures=TRUE, standardized=TRUE)
latent_observed_implied(fit)
usethis::use_data(noloadings)
