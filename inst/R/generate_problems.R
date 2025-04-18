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
#summary(fit, fit.measures=TRUE, standardized=TRUE)
#visualize(fit, plot="latent")
#implied_measurement(fit, "latent_y")
sem_a = fit
usethis::use_data(sem_a, overwrite=TRUE)




# nonlinear relationship between two latents
y_latent_sq = y_latent -.6*x_latent^2
y_vars_sq = generate_latent(vars=4, loading=runif(4, .6, .8), n=1000, y_latent_sq)
names(y_vars_sq) = paste0("y", 1:4)
d = cbind(x_vars, y_vars_sq)
fit = cfa(model, d)
#summary(fit, fit.measures=TRUE, standardized=TRUE)
#implied_measurement(fit, "latent_x")
plot = implied_measurement(fit, "latent_y", method="quadratic") 
plot[[1]] + stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, col="red")
#visualize(fit, subset=1:4)
#visualize(fit, subset=1:4, plot="latent")
  # nonlinear between latents will show up as:
    # nonlinear relationship between observed of y_i
    # wonky bullet shape in y_i/x_i, as well as latent plots
    # nonlinear in implied_measurement between y_i
#visualize(sem_b, plot="latent")
  
sem_b = fit
usethis::use_data(sem_b, overwrite=TRUE)



y_vars_sq = generate_latent(vars=4, loading=runif(4, .6, .8), n=1000, y_latent_sq)
names(y_vars_sq) = paste0("y", 1:4)
y_vars_sq = y_vars_sq + .2*x_vars^2
d = cbind(x_vars, y_vars_sq)
fit = cfa(model, d)
#summary(fit, fit.measures=TRUE, standardized=TRUE)
#implied_measurement(fit, "latent_x")
plot = implied_measurement(fit, "latent_y", method="quadratic") 
plot[[1]] + stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, col="red")
#visualize(fit, plot="latent")
#visualize(fit, subset=1:4)
#
# nonlinear between latents will show up as:
# nonlinear relationship between observed of y_i
# wonky bullet shape in y_i/x_i, as well as latent plots
# nonlinear in implied_measurement between y_i
#visualize(sem_b, plot="latent")

sem_b = fit
usethis::use_data(sem_b, overwrite=TRUE)


# variable that doesn't load on any factor (but it is correlated with others)
x_latent = rnorm(1000)
x_vars_extra = x_vars
x_vars_extra$x5 = with(x_vars, .6*x4 + rnorm(length(x_latent), 0, sqrt(1-.6^2)))
d = cbind(x_vars_extra, y_vars)
model = "
latent_x =~ x1 + x2 + x3 + x4 + x5
latent_y =~ y1 + y2 + y3 + y4
latent_x~~latent_y
"
fit = cfa(model, d)
sem_c = fit
usethis::use_data(sem_c)

cov2cor(sem_c@Model@GLIST$psi)


#summary(fit)
visualize(fit, subset=1:3) #c("x5", "x4", "x1"))
  # model underestimates relationship between x4/x5
#implied_measurement(fit, "latent_x")
  # red = actual, blue = implied
  # model overestimates relationship between x_i/latentx
#implied_measurement(fit, "latent_y")
  # model underestimates relationship between y_i/latenty



# variable that loads on two factors
n = 1500
x_latent = rnorm(n)
y_latent = .4*x_latent + rnorm(length(x_latent), 0, sqrt(1-.4^2))
x_vars = generate_latent(vars=3, loading=c(.3, .4, .5), n=n, x_latent) 
y_vars = generate_latent(vars=3, loading=c(.3, .4, .3), n=n, y_latent)
names(y_vars) = paste0("y", 1:3)
d = cbind(x_vars, y_vars)
d$y3 = with(d, y3 + .8*x_latent)
model = "
latent_x =~ x1 + x2 + x3
latent_y =~ y1 + y2 + y3
latent_y ~~ latent_x
"
# model2 = "
# latent_x =~ x1 + x2 + x3 + x4 + y4
# latent_y =~ y1 + y2 + y3 + y4
# latent_y ~~ latent_x
# "
fit = cfa(model, d)
#visualize(fit, subset=1:5, method="lm")
#visualize(fit, plot="latent")
implied_measurement(fit, "latent_x")
implied_measurement(fit, "latent_y")
visualize(fit, sort_plots = F, method="lm")
#lavInspect(fit, what="cor.ov")-lavInspect(fit2, what="cor.ov") %>% round(digits=2)
  #visualize: consistently underestimating the relationship between y_is
  #implied_measurement: consistently overestimating y_i to latent x
sem_d = fit
usethis::use_data(sem_d)


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
fit = cfa(model, d)
# summary(fit, fit.measures=TRUE, standardized=TRUE)
# visualize(fit, subset=1:5, method="lm")
# visualize(fit, plot="latent")
# implied_measurement(fit, "latent_x", limit=6)
# implied_measurement(fit, "latent_z")
# implied_measurement(fit, "latent_y")
  # implied_measurement: consistently overestimate z_i/x_i and latent_x
  # implied_measurement: consistently overestimate x_i and latent_z
  # implied_measurement: consistently overestimate y_i and latent_y
  # visualize: x_i/z_i are overestimated
  # latent isn't helpful
sem_e = fit
usethis::use_data(sem_e)


# residual correlation between observed
x_latent = rnorm(1000)
y_latent = .3*x_latent + rnorm(length(x_latent), 0, sqrt(1-.1^2))
x_vars = generate_latent(vars=3, loading=runif(4, .6, .8), n=1000, x_latent) 
y_vars = generate_latent(vars=3, loading=runif(4, .6, .8), n=1000, y_latent)
names(y_vars) = paste0("y", 1:3)
y_vars$y1 = y_vars$y1 + .3*x_vars$x1
d = cbind(x_vars, y_vars)
model = "
latent_x =~ x1 + x2 + x3 
latent_y =~ y1 + y2 + y3
latent_x~~latent_y
latent_x ~~ latent_x
latent_y ~~ latent_y
"
fit = cfa(model, d)
#summary(fit, fit.measures=TRUE, standardized=TRUE)
#visualize(fit)
#implied_measurement(fit, "latent_y")
sem_f = fit
usethis::use_data(sem_f, overwrite=TRUE)

# interaction between observed variables
x_latent = rnorm(1000)
y_latent = .3*x_latent + rnorm(length(x_latent), 0, sqrt(1-.1^2))
x_vars = generate_latent(vars=3, loading=runif(4, .6, .8), n=1000, x_latent) 
y_vars = generate_latent(vars=3, loading=runif(4, .6, .8), n=1000, y_latent)
names(y_vars) = paste0("y", 1:3)
y_vars$y1 = y_vars$y1 + .3*y_vars$y2*y_vars$y3
d = cbind(x_vars, y_vars)
model = "
latent_x =~ x1 + x2 + x3 
latent_y =~ y1 + y2 + y3
latent_x~~latent_y
latent_x ~~ latent_x
latent_y ~~ latent_y
"
fit = cfa(model, d)
#summary(fit, fit.measures=TRUE, standardized=TRUE)
#visualize(fit)
#implied_measurement(fit, "latent_y")
sem_g = fit
usethis::use_data(sem_g, overwrite=TRUE)
