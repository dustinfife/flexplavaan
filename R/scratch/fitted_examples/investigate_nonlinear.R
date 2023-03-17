#require(flexplavaan)

# why are two variables showing a nonlienar relationship?
# three options:
# 1. Nonlinear between observed and latent, but both normally distributed
# 2. Nonlinear between two latents
# 3. Skew in each latent variable, but linear relationship
# let's simulate each of these
latent_function = function(a, f) {
  return(a*f + rnorm(length(f), 0, sqrt(1-a^2)))
}
### make functions
logistic = function(x, max, slope, intercept){
  max/(1 + exp(-1*slope*(x-intercept)))
}


# 1. Nonlinear between observed and latent, but both normally distributed
f1 = rnorm(800)
x1 = logistic(f1, 10, .85, 3) + rnorm(length(f1), 0, .25)
x2 = logistic(f1, 10, .95, 3) + rnorm(length(f1), 0, .25)
x3 = logistic(f1, 10, .75, 3) + rnorm(length(f1), 0, .25)

f2 = rnorm(800)
y1 = logistic(f1, 10, .85, 3) + rnorm(length(f1), 0, .25)
y2 = logistic(f1, 10, .95, 3) + rnorm(length(f1), 0, .25)
y3 = logistic(f1, 10, .75, 3) + rnorm(length(f1), 0, .25)

d1 = data.frame(x1, x2, x3, y1, y2, y3)

require(flexplavaan)
require(lavaan)


### fit models
lavfit1 = cfa(model, d1)
residual_plots(lavfit1)
a1 = visualize(lavfit1) + labs(title = "Nonlinear observed")
a2 = implied_measurement(lavfit1) + labs(title = "Nonlinear observed")


# 2. Nonlinear between two latents
f1 = rnorm(800)
f2 = logistic(f1, 10, .85, 3) + rnorm(length(f1), 0, .25)
plot(f1, f2)

factors = function(slope, latent) {
  return(slope*latent + rnorm(length(latent), 0, sqrt(1-latent^2)))
}
x1 = factors(.8, f1)
x2 = factors(.85, f1)
x3 = factors(.9, f1)
y1 = factors(.8, f1)
y2 = factors(.85, f1)
y3 = factors(.9, f1)
d2 = data.frame(x1, x2, x3, y1, y2, y3)

model = "
f1 =~ x1 + x2 + x3
f2 =~ y1 + y2 + y3
"

### fit models
lavfit2 = cfa(model, d)
residual_plots(lavfit2)
b1 = visualize(lavfit2) + labs(title = "Nonlinear latent")
b2 = implied_measurement(lavfit2) + labs(title = "Nonlinear latent")

require(patchwork)
a1 
b1
a2 
b2
# it seems like the nonlinearity is more severe when it occurs between observed
# otherwise, they look the same

skewed = function(slope, latent) {
  return((slope*latent) + rgamma(length(latent), 1))
}

# 3. Skew in each latent variable, but linear relationship
f1 = rgamma(1000, 1)
f2 = .8*f1 + rgamma(1000, .5)

x1 = skewed(.8, f1)
x2 = skewed(.85, f1)
x3 = skewed(.9, f1)
y1 = skewed(.8, f1)
y2 = skewed(.85, f1)
y3 = skewed(.9, f1)
d3 = data.frame(x1, x2, x3, y1, y2, y3)
hist(x1)
### fit models
lavfit3 = cfa(model, d3)
residual_plots(lavfit3)
c1 = visualize(lavfit3) + labs(title = "Skewed latent")
c2 = implied_measurement(lavfit3) + labs(title = "Nonlinear latent")
c1
c2
# not much nonlinearity, but certainly weird looking distributions