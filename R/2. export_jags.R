    ### this script fits three jags models: 
        # 1. the nonlinear variable (x3a) with a correctly specified nonlinear function
        # 2. the nonlinear variable (x3a) with an INcorrectly specified LINEAR function
        # 3. the linear variable (x3B) with a correctly specified linear function

require(blavaan)
require(lavaan)
d = read.csv("data/exp_data.csv")
future::plan("multiprocess")
export.model = FALSE

# this first model uses the nonlinear version of X3/Y3
model.nonlinear = '
  A =~ NA*x1 + x2 + x3a
  A ~~ 1*A
  B =~ NA*y1 + y2 + y3a
  B ~~ 1*B 
  B ~ A
'
# this first model uses the linear version of X3/Y3
model.linear = '
  A =~ NA*x1 + x2 + x3b
  A ~~ 1*A
  B =~ NA*y1 + y2 + y3b
  B ~~ 1*B 
  B ~ A
'
install.packages("runjags")
## fit the nonlinear dataset with nonlinear equation
fit.custom.nonlinear = bcfa(model.linear, data=d,
                           jagcontrol=list(method="rjparallel"),
                           mcmcextra = list(monitor="eta"),
                           target = "jags")
saveRDS(fit.custom.nonlinear, file="data/custom_bayes_fit_linear.rds")

### first model runs quick in jags (to just export syntax)
if (export.model){
  fit.bayes.export = bcfa(model.nonlinear, data=d, mcmcfile=T,
                        target="jags",
                        n.chains = 1,
                        burnin = 10,
                        sample = 20)
}  
### add syntax to do the nonlinear model
extra.fit = "
for (i in 1:N){
  mu_2[i,3] <- nu[3,1,g[i]]^eta[i,1]
  x3a_2[i] ~ dnorm(mu_2[i,3], 1/theta[3,3,g[i]])
  mu_2[i,6] <- nu[6,1,g[i]]^eta[i,2]
  y3a_2[i] ~ dnorm(mu_2[i,6], 1/theta[6,6,g[i]])  
  mu2_eta[i,2] <- alpha[2,1,g[i]] + beta[2,1,g[i]]*eta[i,1]
}"

## fit a standard lavaan model
fit.lavaan = cfa(model.linear, data=d)
saveRDS(fit.lavaan, file="data/fit_lavaan.rds")

## fit the linear dataset assuming a linear models
fit.linear = bcfa(model.linear, data=d)
saveRDS(fit.linear, file="data/initial_fit_linear.rds")

# ## fit the nonlinear dataset with nonlinear equation
# fit.custom.nonlinear = bcfa(model.nonlinear, data=d, 
#                            jagcontrol=list(method="rjparallel"),
#                            mcmcextra = list(syntax=extra.fit),
#                            target = "jags")
# saveRDS(fit.custom.nonlinear, file="data/custom_bayes_fit_nonlinear.rds")
# 
# ## fit the nonlinear dataset, but assume linear models
# fit.bayes.nonlinear = bcfa(model.nonlinear, data=d)
# saveRDS(fit.bayes.nonlinear, file="data/bayes_fit_assume_linear.rds")

