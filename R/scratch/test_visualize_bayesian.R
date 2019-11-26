require(blavaan); require(flexplot)
fit.lavaan <- readRDS("data/custom_bayes_fit_nonlinear.rds")
d = read.csv("data/exp_data.csv")


etas = dimnames(fit.lavaan@external$mcmcout$mcmc[[1]])[[2]]
etas = gsub("beta", "xxxx", etas)
etas = gsub("theta", "xxxx", etas)
etas = grep("eta", etas, value = T)
eta1 = grep(",1]", etas, fixed=T, value=T)
eta2 = grep(",2]", etas, fixed=T, value=T)
d = data.frame(1:300)
d$A = apply(fit.lavaan@external$mcmcout$mcmc[[1]][,eta1],2, median) 
d$B = apply(fit.lavaan@external$mcmcout$mcmc[[1]][,eta2],2, median) 
flexplot(A~B, data=d)














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
fit.lavaan = bcfa(model_correct, data=crossloadings_small)

    #### use estimate x1/x2 to compute quadriture points for each section
str(fit.lavaan)

etas = dimnames(fit.lavaan@external$mcmcout$mcmc[[1]])[[2]]
etas = gsub("beta", "xxxx", etas)
etas = gsub("theta", "xxxx", etas)
etas = grep("eta", etas, value = T)
eta1 = grep(",1]", etas, fixed=T, value=T)
eta2 = grep(",2]", etas, fixed=T, value=T)
d$A = apply(fit.lavaan@external$mcmcout$mcmc[[1]][,eta1],2, median) 
d$B = apply(fit.lavaan@external$mcmcout$mcmc[[1]][,eta2],2, median) 



require(lavaan)
fit.lavaan = cfa(model_correct, data=crossloadings_small)
fit.lavaan2 = cfa(model_missspecified, data=crossloadings_small)
#viz_diagnostics(data = crossloadings_small, mapping = aes(x1,y1), fit.lavaan = fit.lavaan, fit.lavaan2 = fit.lavaan2, plot="trace")
visualize(fit.lavaan, fit.lavaan2)
coef(fit.lavaan)
coef(fit.lavaan2)


    #### a plot of factor scores sorted by size for both models
preds1 = lavPredict(fit.lavaan)
preds2 = lavPredict(fit.lavaan2)
predicted.values = data.frame(rbind(preds1, preds1))
names(predicted.values) = c("f1", "f2")
predicted.values$model = c(rep("model1", times=nrow(preds1)), rep("model2", times=nrow(preds2)))
head(predicted.values)
flexplot(f2~f1 + model, data=predicted.values)
latent = predicted.values %>% 
  tidyr::gather("latent.variable", "score", f1:f2) %>% 
  dplyr::arrange(score) %>% 
  dplyr::mutate(index=1:nrow(.))

flexplot(score~index + model | latent.variable, data=latent, suppress_smooth = T)
