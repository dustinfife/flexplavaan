#require(flexplavaan)

# why are two variables showing a nonlienar relationship?
# three options:
# 1. Nonlinear between observed and latent, but both normally distributed
# 2. Nonlinear between two latents
# 3. Nonlinear in error terms (i.e.)
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

d = data.frame(x1, x2, x3, y1, y2, y3)

require(flexplavaan)
require(lavaan)

model = "
f1 =~ x1 + x2 + x3
f2 =~ y1 + y2 + y3
"

### fit models
lavfit = cfa(model, d)
residual_plots(lavfit)
visualize(lavfit)
implied_measurement(lavfit)
