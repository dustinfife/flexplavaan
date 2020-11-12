require(lavaan)
require(tidyverse)
source("inst/R/generate_functions.R")


# variable that doesn't load on any factor (but it is correlated with others)
x_latent = rnorm(1000)
y_latent = .1*x_latent + rnorm(length(x_latent), 0, sqrt(1-.1^2))
x_vars = generate_latent(vars=4, loading=runif(4, .4, .7), n=1000, x_latent) 
y_vars = generate_latent(vars=4, loading=runif(4, .4, .7), n=1000, y_latent)
names(y_vars) = paste0("y", 1:4)
x_vars$x5 = with(x_vars, .6*x4 + rnorm(length(x_latent), 0, sqrt(1-.6^2)))
d = cbind(x_vars, y_vars)
head(d)
model = "
latent_x =~ x1 + x2 + x3 + x4 + x5
latent_y =~ y1 + y2 + y3 + y4
latent_x~~latent_y
"
model_2 = "
latent_x =~ x1 + x2 + x3 + x4
latent_y =~ y1 + y2 + y3 + y4
x5 ~ x4
latent_x~~latent_y
"
fit = cfa(model, d)
fit2 = cfa(model_2, d)
summary(fit, fit.measures=TRUE, standardized=TRUE)
implied_measurement(fit2, "latent_y")


# weak associations
x_vars = generate_latent(vars=6, loading=.1, n=1000) 
model = "
latent =~ x1 + x2 + x3 + x4 + x5 + x6
"
fit = cfa(model, x_vars)
summary(fit, fit.measures=TRUE, standardized=TRUE)
visualize(fit)




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
implied_measurement(fit, "latent_y")

summary(fit, fit.measures=TRUE, standardized=TRUE)
#visualize(fit, subset=1:4)
#visualize(fit, plot="measurement")
### need a plot that views each variable against latent, even those not modeled
k = lavPredict(fit, type="lv")
d2 = cbind(d, k)
a1 = added.plot(latent_y~latent_x + x4, data=d2, method="lm")
a2 = added.plot(latent_y~latent_x + x3, data=d2, method="lm")
a3 = added.plot(latent_y~latent_x + x2, data=d2, method="lm")
a4 = added.plot(latent_y~latent_x + x1, data=d2, method="lm")
cowplot::plot_grid(a1, a2, a3, a4)
d2$residuals = residuals(lm(latent_y~latent_x, data=d2))
flexplot(residuals~x3, data=d2)



# variable that loads on two factors
n = 800
x_latent = rnorm(n)
y_latent = .4*x_latent + rnorm(length(x_latent), 0, sqrt(1-.4^2))
x_vars = generate_latent(vars=4, loading=runif(4, .4, .7), n=n, x_latent) 
y_vars = generate_latent(vars=4, loading=runif(4, .4, .7), n=n, y_latent)
names(y_vars) = paste0("y", 1:4)
d = cbind(x_vars, y_vars)
d$y4 = with(d, y4 + .8*x_latent)
d$y_latent = y_latent
d$x_latent = x_latent
flexplot(y4~x_latent, data=d)
flexplot(y3~x_latent, data=d)
model = "
latent_x =~ x1 + x2 + x3 + x4
latent_y =~ y1 + y2 + y3 + y4
latent_y ~~ latent_x
"
fit = cfa(model, d)
summary(fit, fit.measures=TRUE, standardized=TRUE)
#visualize(fit, subset=1:4)
#visualize(fit, plot="measurement")
### need a plot that views each variable against latent, even those not modeled
k = lavPredict(fit, type="lv")
d2 = cbind(d, k)
flexplot(y3~latent_x, data=d2)
flexplot(y4~latent_x, data=d2)

# this should show things looking good
cov_latent_observed = fit@Model@GLIST$lambda %*% fit@Model@GLIST$psi
slope = cov_latent_observed[3,2]
int = mean(d2$latent_y) - slope*mean(d2$x3)
flexplot(latent_y~x3, data=d2) +
  geom_abline(slope = slope, intercept = int)

# this should show problems!
slope = cov_latent_observed[8,1]
int = mean(d2$latent_x) - slope*mean(d2$y4)
flexplot(latent_x~y4, data=d2) +
  geom_abline(slope = slope, intercept = int)
  # model implies a stronger relationship than there actuall is!




a1 = added.plot(latent_y~latent_x + x4, data=d2, method="lm")
a2 = added.plot(latent_y~latent_x + x3, data=d2, method="lm")
a3 = added.plot(latent_y~latent_x + x2, data=d2, method="lm")
a4 = added.plot(latent_y~latent_x + x1, data=d2, method="lm")
cowplot::plot_grid(a1, a2, a3, a4)
d2$residuals = residuals(lm(latent_y~latent_x, data=d2))
flexplot(residuals~x3, data=d2)


# missing association between latents
n = 500
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
visualize(fit, subset=1:5, method="lm")





require(lavaan)
require(tidyverse)
source("inst/R/generate_functions.R")


beta = .5
b = .3
a = .6
n = 10000
vy = 2
beta_unst = beta*sqrt(vy)
X = rnorm(n)
Y = beta_unst*X + rnorm(n, 0, sqrt(vy-beta_unst^2))
var(Y)
cor(X,Y)

# weak associations
x_vars = generate_latent(vars=4, loading=a, n=n, X) 
y_vars = generate_latent(vars=4, loading=b, n=n, Y) 
names(y_vars) = paste0("y", 1:4)
d = cbind(x_vars, y_vars, X, Y)
model = "
latent_x =~ x1 + x2 + x3 + x4
latent_y =~ y1 + y2 + y3 + y4
latent_y ~~ latent_x
"
head(d)
fit = cfa(model, d)
latent_observed_implied(fit)
cov(d$X, d$Y)
cov(d)[lavNames(fit),c("X", "Y")]


fit@Model@GLIST


cov(d)[1,7]
A = 
a*beta
cor(d)

model = "
latent =~ x1 + x2 + x3 + x4 + x5 + x6
"
fit = cfa(model, x_vars)
summary(fit, fit.measures=TRUE, standardized=TRUE)
visualize(fit)

# variable that doesn't load on any factor (but it is correlated with others)
x_latent = rnorm(1000)
y_latent = .1*x_latent + rnorm(length(x_latent), 0, sqrt(1-.1^2))
x_vars = generate_latent(vars=4, loading=runif(4, .4, .7), n=1000, x_latent) 
y_vars = generate_latent(vars=4, loading=runif(4, .4, .7), n=1000, y_latent)

