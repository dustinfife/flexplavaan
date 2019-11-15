  ### this script reads in the regular CFA and does diagnostic plots
require(tidyverse); require(blavaan); require(flexplot)
source("R/functions_vizsem.R")
d = read.csv("data/exp_data.csv")



# linear model ------------------------------------------------------------

  ### read in blavaan data
fit.bayes.linear <- readRDS("data/initial_fit_linear.rds")

  ## factor scores (estimated during MCMC)
d$f_linear = lavPredict(fit.bayes.linear)

  ## make sure estimated factor scores are similar to actual factor scores
flexplot(latent~f_linear, data=d)
flexplot(x1~latent, data=d)
flexplot(x1~f_linear, data=d)

  ## create diagnostic plots
diagnostic_plots(x="x3b", y="x2", latent = "f_linear", data=d, object=fit.bayes.linear, plot="model") 
diagnostic_plots("x3b", "x2", "latent", data=d, object=fit.bayes.linear, plot="trace") + geom_smooth(method="lm")
diagnostic_plots("x3b", "x2", "latent", data=d, object=fit.bayes.linear, plot="residuals")



# nonlinear model ---------------------------------------------------------

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
