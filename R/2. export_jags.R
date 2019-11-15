require(blavaan)
d = read.csv("data/exp_data.csv")
future::plan("multiprocess")

### estimate fit with lavaan
model.nonlinear = '
  force =~ NA*x1 + x2 + x3a
  force ~~ 1*force
'
model.linear = '
  force =~ NA*x1 + x2 + x3b
  force ~~ 1*force
'

    ### first model runs quick in jags (to just export things)
fit.bayes.export = bcfa(model.nonlinear, data=d, mcmcfile=T, 
                 target="jags", 
                 n.chains = 1,
                 burnin = 100,
                 sample = 2000)
    
  ### second model 
fit.bayes.linear = bcfa(model.linear, data=d, save.lvs = T)
saveRDS(fit.bayes.linear, file="data/initial_bayes_fit_linear.rds")

fit.bayes.nonlinear = bcfa(model.nonlinear, data=d, save.lvs = T)
saveRDS(fit.bayes.nonlinear, file="data/initial_bayes_fit_nonlinear.rds")
