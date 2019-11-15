### this script reads in the regular CFA and does diagnostic plots
require(tidyverse); 
require(blavaan); 
require(flexplot)
source("R/functions_vizsem.R")
d = read.csv("data/exp_data.csv")


### read in blavaan data
fit.bayes.nonlinear <- readRDS("data/custom_fit_nonlinear.rds")

## factor scores (estimated during MCMC)
d$f_nonlinear = fit.bayes.nonlinear$results[12:nrow(results),2]

## make sure estimated factor scores are similar to actual factor scores
flexplot(latent~f_nonlinear, data=d)
flexplot(x1~latent, data=d)
flexplot(x1~f_nonlinear, data=d)

diagnostic_plots("x3a", "x2", "f_nonlinear", data=d, object=fit.bayes.nonlinear, plot="model")
diagnostic_plots("x3a", "x2", "f_nonlinear", data=d, object=fit.bayes.nonlinear, plot="trace")
diagnostic_plots("x3a", "x2", "f_nonlinear", data=d, object=fit.bayes.nonlinear, plot="residuals")

diagnostic_plots("x3b", "x2", "latent", data=d, object=fit.bayes.linear, plot="residuals")

?blavaan
