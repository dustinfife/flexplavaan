require(lavaan)
require(flexplot)
data(hogwarts_survival)
head(hogwarts_survival)
model = "
magic_knowledge =~ potions + history + herbology
magic_skills =~ spells + darkarts + flying 
magic_skills ~ magic_knowledge
"
hogwarts_fit = cfa(model, hogwarts_survival)
summary(hogwarts_fit, fit.measures=TRUE)
visualize(hogwarts_fit, method="loess", sample = 500)
visualize(hogwarts_fit, method="loess", subset = 4:6, sample = 300)
visualize(hogwarts_fit, method="loess", subset = 1:3)


    #### fit again with nonlinear model
### export the jags syntax (to make it easier to edit)
require(blavaan)
fit.bayes.export = bcfa(model, data=hogwarts_survival, 
                        mcmcfile="hogwarts_survival",
                        target="jags",
                        n.chains = 1,
                        burnin = 1,
                        sample = 2, 
                        adapt=1)

### regex the file exported
full.file = "hogwarts_survival/sem.jag"
fl = readChar(full.file, file.info(full.file)$size)
old = c("mu[i,5] <- nu[5,1,g[i]] + lambda[5,2,g[i]]*eta[i,2]",
        "nu[6,1,1] <- parvec[19]",
        "parvec[19] ~ dnorm(0,1e-3)")
new = c("mu[i,5] <- mx/(1 + exp(-1*lambda[5,2,g[i]]*(eta[i,2] - nu[5,1,g[i]])))",
        "nu[6,1,1] <- parvec[19]\n mx <- parvec[20]",
        "parvec[19] ~ dnorm(0,1e-3)\n parvec[20] ~ dunif(20, 100)")

fl_new = fl
for (i in 1:length(old)){
  fl_new = gsub(old[i], new[i], fl_new, fixed=T)
}
fileConn<-file("hogwarts_survival/sem_nonlinear.jag")
writeLines(fl_new, fileConn)
close(fileConn)

### refit the model
load("hogwarts_survival/semjags.rda")
jagtrans$data$mx = NA
names(jagtrans$data)
#jagtrans$monitors = jagtrans$monitors[-c(1,4, 22,23)]
jagtrans$monitors = c(jagtrans$monitors, "eta", "mx") ### monitor the latent variable
hogwarts_nonlinear <- run.jags("hogwarts_survival/sem_nonlinear.jag", 
                                 monitor = jagtrans$monitors,
                                 data = jagtrans$data, 
                                 method = "parallel",
                                 n.chains = 2)
saveRDS(hogwarts_nonlinear, file="data/hogwarts_nonlinear.rds")
hogwarts_summary = summary(hogwarts_nonlinear)
saveRDS(hogwarts_summary, file="data/hogwarts_summary.rds")
