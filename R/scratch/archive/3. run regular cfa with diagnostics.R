  ### this script reads in the CFA models and does diagnostic plots
require(tidyverse); require(blavaan); require(flexplot); require(lavaan)
source("R/functions_vizsem.R")
d = read.csv("data/exp_data.csv")


# standard lavaan model ---------------------------------------------------
    #### plot residuals against residuals (x2-x2' versus x1-x1')

### read in lavaan data
fit.lavaan <- readRDS("data/fit_lavaan.rds")
observed.vars = lavNames(fit.lavaan, type="ov")
lav_residuals = d[,observed.vars] - lavPredict(fit.lavaan, type="ov")

flexplot(x3b~x2, data=lav_residuals, method="lm")

residuals(fit.lavaan)
names(fit.lavaan)
?blavaan
@external$mcmc)



?bcfa











# generate predictions from mcmc object -----------------------------------


names(fit.lavaan@external$mcmcout)

str(fit.lavaan@external$mcmcout$mcmc)


etas = dimnames(fit.lavaan@external$mcmcout$mcmc[[1]])[[2]]
etas = gsub("beta", "xxxx", etas)
etas = gsub("theta", "xxxx", etas)
etas = grep("eta", etas, value = T)
eta1 = grep(",1]", etas, fixed=T, value=T)
eta2 = grep(",2]", etas, fixed=T, value=T)
d$A = apply(fit.lavaan@external$mcmcout$mcmc[[1]][,eta1],2, median) 
d$B = apply(fit.lavaan@external$mcmcout$mcmc[[1]][,eta2],2, median) 
      

## factor scores (estimated during MCMC)
#head(lavPredict(fit.lavaan, method="Bartlett"))
#d[,c("A", "B")] = lavPredict(fit.lavaan)
  flexplot(B~A, data=d)
  flexplot(latent_b~latent_a, data=d)
  flexplot(A~latent_a, data=d)
  flexplot(B~latent_b, data=d)
    ### it's doing pretty good!
  cor(d$B, d$latent_b)
  
## residualize X2 based on latent variable
  flexplot(x2~A, data=d)
d$x2_residual = residuals(lm(x2~A+B, data=d))
d$x1_residual = residuals(lm(x1~A+B, data=d))
flexplot(x2~x1_residual, data=d, method="lm")
cor(d$x2_residual, d$x1)

flexplot(x2~x1 | A, data=d, method="lm")

attr(, "sim")$samples
str(attr(fit.lavaan, "external"))
str(attr(fit.lavaan, "stansumm"))$mcmcout
slots(attr(fit.lavaan, "external")$mcmcout)


str(fit.lavaan@external$mcmcout@sim$samples[[1]])
attr(fit.lavaan@external$mcmcout@sim$samples[[1]],"mean_pars")

attr(attr(fit.lavaan, "external")$mcmcout, "model_pars")
e = d %>% gather(key="estimate", value="value", x2_residual, x2)
flexplot(value~x1 | estimate, data=e, method="lm", ghost.line="black")
    ### interesting....we subtract out too much! the relationship becomes negative
residuals(fit.lavaan)



residuals(fit.lavaan)

with(d, cor(x2_residual, x1))
  #### looks beautiful

  #### trace plot (fit between x1 and x2 implied by lavaan)
  #### use lavaan to predict X2 from X1 (which automatically accounts for the latent variable)
pred.matrix = sequence_grid("x1", d)
m = data.frame(x1 = sequence_grid("x1", d), x2 = mean(d$x2)+rnorm(10, 0, .001), x3b = mean(d$x3b)+rnorm(10, 0, .001))
m$latent = as.numeric(lavPredict(fit.lavaan, newdata = m, method="Bartlett"))
m$latent = 29.897 + m$latent * 4.415  
flexplot(x2~x1, data=d, method="lm") +
  geom_line(data=m, aes(x1, latent), col="red")

  #### model plot (fit between x1 and outcome)
flexplot(x1~f_linear, data=d)

  #### compare fits of the actual versus estimated factor line
d %>% tidyr::gather("method", "value", f_linear, latent) %>% 
  mutate(method = factor(method, labels = c("Estimated", "Actual"))) %>% 
  with(flexplot(x1~value | method, data = ., ghost.line="red"))
















## make sure estimated factor scores are similar to actual factor scores
flexplot(latent~f_linear, data=d) 

#### expand that grid
coef(fit.lavaan)
x1 = seq(from=min(d$x1), to = max(d$x1), length.out=20)
f = (x1 - 9.983)/1.506
x2 = 4.415*f + 29.897 


newd = data.frame(x1=x1, x2=x2, f=f)
flexplot(x2~x1, data=d, method="lm") +
  geom_line(data=newd, aes(x1,x2))





new_data = c("x1", "x2", "x3b") %>% map(sequence_grid, data=d) %>% expand.grid
names(new_data) = c("x1", "x2", "x3b")
new_data$prediction = predict(fit.lavaan, new_data) %>% fifer::rescale(new.mean = mean(d$x2), new.sd = sd(d$x2))
new_data$model = "latent"

flexplot(x2~x3b | x1, data=d, prediction = new_data, method="lm")



## create diagnostic plots
diagnostic_plots(x="x3b", y="x2", latent = "latent", data=d, object=fit.bayes.linear, plot="model")
diagnostic_plots("x3b", "x2", "latent", data=d, object=fit.bayes.linear, plot="trace") + geom_smooth(method="lm")
diagnostic_plots("x3b", "x2", "latent", data=d, object=fit.bayes.linear, plot="residuals")


    #### do a plot that subtracts from y the effect of the third variable
d$x2_resid = residuals(lm(x2~x1, data=d)) #### remove the effect of x1
d$x3_resid = residuals(lm(x3b~x1, data=d)) #### remove the effect of x1
predicted = generate_predictions(fit.lavaan, "x3b", data=d)  ### predict value of x3b, assuming all others are constant
predicted$latent = fifer::rescale(predicted[,"latent"],new.mean = mean(d[,"x2_resid"]), new.sd = sd(d[,"x2_resid"]))
predicted$x3_resid = fifer::rescale(predicted[,"x3_resid"],new.mean = mean(d[,"x3_resid"]), new.sd = sd(d[,"x3_resid"]))
names(predicted)
ggplot(d, aes(x3_resid, x2_resid)) +
  geom_point() + 
  geom_line(data=predicted, aes_string("x3b", "latent")) +
  geom_smooth()

# linear model ------------------------------------------------------------

### read in blavaan data
fit.bayes.linear <- readRDS("data/initial_fit_linear.rds")

## factor scores (estimated during MCMC)
d$f_linear = lavPredict(fit.bayes.linear)

## make sure estimated factor scores are similar to actual factor scores
flexplot(latent~f_linear, data=d)
flexplot(x1~latent, data=d)
flexplot(x1~f_linear, data=d)
d %>% tidyr::gather("method", "value", f_linear, latent) %>% 
  mutate(method = factor(method, labels = c("Estimated", "Actual"))) %>% 
  with(flexplot(x1~value | method, data = ., ghost.line="red")) 

## create diagnostic plots
diagnostic_plots(x="x3b", y="x2", latent = "latent", data=d, object=fit.bayes.linear, plot="model")
diagnostic_plots("x3b", "x2", "latent", data=d, object=fit.bayes.linear, plot="trace") + geom_smooth(method="lm")
diagnostic_plots("x3b", "x2", "latent", data=d, object=fit.bayes.linear, plot="residuals")


# nonlinear model (correctly specified) -----------------------------------

### read in blavaan data
custom.bayes <- readRDS("data/custom_bayes_fit_nonlinear.rds")

## factor scores (estimated during MCMC)
d$f_linear = lavPredict(custom.bayes)

## make sure estimated factor scores are similar to actual factor scores
flexplot(latent~f_linear, data=d)
flexplot(x3a~latent, data=d)
flexplot(x3a~f_linear, data=d)
d %>% tidyr::gather("method", "value", f_linear, latent) %>% 
  mutate(method = factor(method, labels = c("Estimated", "Actual"))) %>% 
  with(flexplot(x3a~value | method, data = ., ghost.line="red")) 


## create diagnostic plots
diagnostic_plots(x="x3a", y="x2", latent = "f_linear", data=d, object=custom.bayes, plot="model") 
diagnostic_plots("x3a", "x2", "f_linear", data=d, object=custom.bayes, plot="trace") + geom_smooth(method="lm")
diagnostic_plots("x3a", "x2", "f_linear", data=d, object=custom.bayes, plot="residuals")






# nonlinear model (incorrectly specified) --------------------------------

fit.bayes.nonlinear <- readRDS("data/initial_bayes_fit_nonlinear.rds")

## factor scores (estimated during MCMC)
d$f_nonlinear = lavPredict(fit.bayes.nonlinear)

## make sure estimated factor scores are similar to actual factor scores
flexplot(latent~f_nonlinear, data=d)
flexplot(x1~latent, data=d)
flexplot(x1~f_nonlinear, data=d)

diagnostic_plots("x3a", "x2", "f_nonlinear", data=d, object=fit.bayes.nonlinear, plot="model")
diagnostic_plots("x3a", "x2", "f_nonlinear", data=d, object=fit.bayes.nonlinear, plot="trace")
diagnostic_plots("x3a", "x2", "f_nonlinear", data=d, object=fit.bayes.nonlinear, plot="residuals")



# # residualize the variables (removing the effects of the latent score)
# d$x1_resid = d$x1 - d$f_linear
# d$x2_resid = d$x2 - d$f_linear
# d$x3a_resid = d$x3a - d$f_linear
# d$x3b_resid = d$x3b - d$f_linear
# 
# # add the latent line by transforming the f to be on the same scale as y axis
# d$x1_predicted = fifer::rescale(d$f_linear,new.mean = mean(d$x1), new.sd = sd(d$x1))
# 
# ggplot(data=d, aes(x2, x1, color=f_linear)) +
#   geom_point() +
#   geom_smooth(aes(x=x2, y=x1_predicted))+ ### this is the line from the latent variable
#   theme_bw()
# 
# ### residual plot
# ggplot(data=d, aes(x1, x2_resid, color=f_linear)) +
#   geom_point() +
#   theme_bw() +
#   geom_smooth()
# 
# 
# 
# fit.linear <- readRDS("data/initial_fit_linear.rds")
# d$f_linear = lavPredict(fit.linear)
# flexplot::flexplot(latent~f_linear, data=d)
# flexplot(x1~latent, data=d)
# flexplot(x1~f_linear, data=d)
