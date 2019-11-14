require(blavaan)
d = read.csv("../data/exp_data.csv")

### estimate fit with lavaan
model = '
  force =~ NA*x1 + x2 + x3
  force ~~ 1*force
'

    ### first model runs quick in jags (to just export things)
fit.bayes = bcfa(model, data=d, mcmcfile=T, 
                 target="jags")
    ### second model 
fit.bayes = bcfa(model, data=d, mcmcfile=T, 
                 target="jags")
saveRDS("fit.bayes", file="../data/initial_bayes_fit.rds")
