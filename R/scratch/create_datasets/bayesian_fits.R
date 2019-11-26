data("crossloadings_small")
data("crossloadings_large")
cor(crossloadings_small)
model_correct = "
f1 =~ x1 + x2 + x3 + y3
f2 =~ y1 + y2 + y3
f1 ~ f2
"
model_missspecified = "
f1 =~ x1 + x2 + x3
f2 =~ y1 + y2 + y3
f1 ~ f2
"

require(blavaan)
bayes_fit_crossloadings = bcfa(model_missspecified, data=crossloadings_small,
                            mcmcextra = list(monitor=c("eta", "mu")))

saveRDS(fit.custom.nonlinear, file="data/custom_bayes_fit_linear.rds")


vignette(package="blavaan")

load("lavExport/semstan.rda")
names(stantrans$data)
stantrans$monitors = c(stantrans$monitors, "u")
require(rstan)
fit = stan("lavExport/semstan.rda")
blavaan
> fit <- run.jags("lavExport/sem.jag", monitor = jagtrans$coefvec$jlabel,
                  + data = jagtrans$data, inits = jagtrans$inits)