  ### this script reads in the regular CFA and does diagnostic plots
require(tidyverse); require(blavaan)
source("R/functions_vizsem.R")
d = read.csv("data/exp_data.csv")

fit.bayes.linear <- readRDS("data/initial_bayes_fit_linear.rds")
fit.bayes.nonlinear <- readRDS("data/initial_bayes_fit_nonlinear.rds")

  ## factor scores

d$f_linear = blavInspect(fit.bayes.linear, "lvmeans") 
d$f_nonlinear =blavInspect(fit.bayes.nonlinear, "lvmeans") 


diagnostic_plots("x3a", "x2", "f_nonlinear", data=d, object=fit.bayes.nonlinear, plot="model")
diagnostic_plots("x3a", "x2", "f_nonlinear", data=d, object=fit.bayes.nonlinear, plot="trace")
diagnostic_plots("x3a", "x2", "f_nonlinear", data=d, object=fit.bayes.nonlinear, plot="residuals")
diagnostic_plots(x="x3b", y="x2", latent = "f_linear", data=d, object=fit.bayes.linear, plot="model")
diagnostic_plots("x3b", "x2", "f_linear", data=d, object=fit.bayes.linear, plot="trace")
diagnostic_plots("x3b", "x2", "f_linear", data=d, object=fit.bayes.linear, plot="residuals")


# residualize the variables (removing the effects of the latent score)
d$x1_resid = d$x1 - d$f_linear
d$x2_resid = d$x2 - d$f_linear
d$x3a_resid = d$x3a - d$f_linear
d$x3b_resid = d$x3b - d$f_linear

# add the latent line by transforming the f to be on the same scale as y axis
d$x1_predicted = fifer::rescale(d$f_linear,new.mean = mean(d$x1), new.sd = sd(d$x1))

ggplot(data=d, aes(x2, x1, color=f_linear)) +
  geom_point() +
  geom_smooth(aes(x=x2, y=x1_predicted))+ ### this is the line from the latent variable
  theme_bw()

### residual plot
ggplot(data=d, aes(x1, x2_resid, color=f_linear)) +
  geom_point() +
  theme_bw() +
  geom_smooth()
