    #### this script fits the model from step 4 using raw jags (without blavaan)

require(blavaan)
require(tidyverse)
require(runjags)
#source("R/functions_vizsem.R")
d = read.csv("data/exp_data.csv")
data(nonlinear)


load("lavExport/semjags.rda")
names(jagtrans$data)[7:8] = c("max", "midpoint")

jagtrans$monitors = c(jagtrans$monitors, "eta") ### monitor the latent variable
jagtrans$monitors[8:10] = gsub("nu", "max", jagtrans$monitors[8:10])
jagtrans$monitors[1:3] = gsub("lambda", "midpoint", jagtrans$monitors[1:3])
fit.nonlinear.custom <- run.jags("lavExport/sem.jag", monitor = jagtrans$monitors,
                                 data = jagtrans$data, method = "parallel",
                                 n.chains = 3)
results = summary(fit.nonlinear.custom)
fit.lavaan = results




fit.nonlinear.custom$results = results
saveRDS(fit.nonlinear.custom, file="data/custom_fit_nonlinear.rds")



# model.nonlinear = '
#   force =~ NA*x1 + x2 + x3a
#   force ~~ 1*force
# '
# model.linear = '
#   force =~ NA*x1 + x2 + x3b
#   force ~~ 1*force
# '
# fit.custom.nonlinear = cfa(model.linear, data=d, mcmcextra = list(syntax = "mu[i,3] <- nu[3,1,g[i]]^eta[i,1]"))
# saveRDS(fit.custom.nonlinear, file="data/custom_fit_nonlinear.rds")
# ### first model runs quick in jags (to just export things)
# # fit.bayes.export = bcfa(model.nonlinear, data=d, mcmcfile=T, 
# #                  target="jags", 
# #                  n.chains = 1,
# #                  burnin = 100,
# #                  sample = 2000)
# #     
# ### second model 
# #fit.bayes.linear = bcfa(model.linear, data=d, save.lvs = T)
# #saveRDS(fit.bayes.linear, file="data/initial_bayes_fit_linear.rds")
# 
# 
# 
# 
# 
# results[12:nrow(results),2]
# typeof(results)
# dimnames(results)
# typeof(fit.nonlinear.custom)
