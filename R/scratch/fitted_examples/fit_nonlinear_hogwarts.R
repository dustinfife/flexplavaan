require(lavaan)
require(blavaan)
require(flexplot)
require(runjags)

# fit/visualize using standard lavaan -------------------------------------
model = "
knowledge =~ potions + history + herbology
skills =~ spells + darkarts + flying 
skills ~ magic_knowledge
"

hogwarts_fit = sem(model, data=hogwarts_survival)
  summary(hogwarts_fit, fit.measures=TRUE)
hogwarts_viz = visualize(hogwarts_fit, method="loess")
  ggsave(file="plots/hogwarts_linear.jpg", hogwarts_viz)


# write the jags syntax ---------------------------------------------------
require(blavaan)
hogwarts_survival$surv = rnorm(nrow(hogwarts_survival))
model = "
knowledge =~ potions + history + herbology
skills =~ spells + darkarts + flying 
skills ~ knowledge
surv ~ knowledge + skills
"
fit.bayes.export = bcfa(model, data=hogwarts_survival, 
                        mcmcfile="hogwarts_survival",
                        target="jags",
                        n.chains = 1,
                        burnin = 1,
                        sample = 2, 
                        adapt=1)


# modify the jags model ---------------------------------------------------
full.file = "hogwarts_survival/sem.jag"
fl = readChar(full.file, file.info(full.file)$size)
old = c("mu[i,5] <- nu[5,1,g[i]] + lambda[5,2,g[i]]*eta[i,2]",
        "alpha[3,1,1] <- parvec[23]",
        "parvec[23] ~ dnorm(0,1e-3)",
        "surv[i] ~ dnorm(mu[i,7], 1/psi[3,3,g[i]])",
        "mu[i,7] <- alpha[3,1,g[i]] + beta[3,1,g[i]]*eta[i,1] + beta[3,2,g[i]]*eta[i,2]",
        "psi[3,3,1] <- pow(parvec[14],2)")
new = c("mu[i,5] <- mx/(1 + exp(-1*lambda[5,2,g[i]]*(eta[i,2] - nu[5,1,g[i]])))",
        "alpha[3,1,1] <- parvec[23]\n mx <- parvec[24]",
        "parvec[23] ~ dnorm(0,1e-3)\n parvec[24] ~ dunif(20, 100)",
        "survived[i] ~ dbern(mu[i,7])",
        "logit(mu[i,7]) <- alpha[3,1,g[i]] + beta[3,1,g[i]]*eta[i,1] + beta[3,2,g[i]]*eta[i,2]",
        "")
fl_new = fl
for (i in 1:length(old)){
  fl_new = gsub(old[i], new[i], fl_new, fixed=T)
}
fileConn<-file("hogwarts_survival/sem_nonlinear.jag")
writeLines(fl_new, fileConn)
close(fileConn)


# refit the jags model ----------------------------------------------------
load("hogwarts_survival/semjags.rda")
jagtrans$data$mx = NA
jagtrans$data$survived = hogwarts_survival$survived
jagtrans$data$surv = NULL
jagtrans$monitors = c("mx", jagtrans$monitors, "eta") ### monitor the latent variable
hogwarts_nonlinear <- run.jags("hogwarts_survival/sem_nonlinear.jag", 
                                 monitor = jagtrans$monitors,
                                 data = jagtrans$data, 
                                 method = "parallel",
                                 n.chains = 2)
saveRDS(hogwarts_nonlinear, file="data/hogwarts_nonlinear.rds")
hogwarts_summary = summary(hogwarts_nonlinear)
saveRDS(hogwarts_summary, file="data/hogwarts_summary.rds")
