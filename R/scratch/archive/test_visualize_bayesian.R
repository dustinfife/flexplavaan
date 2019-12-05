#bayes_fit_crossloadings_correct = readRDS(file="data/bayes_fit_crossloadings_correct.rds")
require(blavaan); 
require(flexplot); 
require(tidybayes); 
require(fifer); 
require(tidyverse)
require(GGally)


data(nonlinear)

d = nonlinear
fit.custom.nonlinear = readRDS(file="data/nonlinear_fit_bayes.rds")
head(d)
viz_diagnostics(data = d, mapping = aes(X1, X2), 
                fit.lavaan=fit.custom.nonlinear, 
                plot="trace", suppress_smooth=T)

lavNames(fit.custom.nonlinear)
#visualize(custom.bayes)


  
lvs = data.frame(blavInspect(bayes_fit_crossloadings_correct, 'lvmeans'))
names(lvs) = c("f1", "f2")
p = viz_diagnostics(data = crossloadings_large, mapping = aes(x1, x2), 
                fit.lavaan=bayes_fit_crossloadings_correct, plot="trace", suppress_smooth=F)
p 

fit.lavaan = bayes_fit_crossloadings_correct
x = "x2"; y = "x3"
data = crossloadings_large




viz_diagnostics(data = crossloadings_large, mapping = aes(x1, x2), 
                fit.lavaan=bayes_fit_crossloadings_correct, plot="trace")

visualize(bayes_fit_crossloadings_correct)
summary(bayes_fit_crossloadings_correct, standardized=T)



data = cbind(crossloadings_large, lvs) %>% 
  data.frame %>% 
  mutate(f2_new = rescale(f2, mean(x2), sd(x2)))





# Use tidybayes to extract parameters -------------------------------------
parameters = blavInspect(bayes_fit_crossloadings_correct, 'mcmc')

# Convert to tidy format
lambda = parameters %>% spread_draws(ly_sign[i], regex=T) %>% 
  setNames(gsub("ly_sign", "lambda", names(.))) %>% 
  spread(i, lambda) %>% 
  rename_at(vars(as.character(1:5)), ~ paste0("lambda", 1:5))
nu = parameters %>% spread_draws(Nu_free[i], regex=T) %>% 
  setNames(gsub("Nu_free", "nu", names(.))) %>% 
  spread(i, nu) %>% 
  rename_at(vars(as.character(1:6)), ~ paste0("nu", 1:6))
params = left_join(lambda, nu, by=".draw") %>% 
  group_by(.iteration.x) %>% 
  summarise_all(mean)



latents = blavInspect(bayes_fit_crossloadings_correct, 'lvs')
variables = latents %>% spread_draws(eta[i,v]) %>% 
  spread(v, eta) %>% 
  dplyr::rename(eta1 = `1`, eta2 = `2`) %>% 
  group_by(.iteration, i) %>% 
  summarise_all(mean) %>% 
  nest(eta1, eta2) 

nn = left_join(params, variables, by = c(".iteration.x" = ".iteration"))

customfunc = function(data, name, lambda1, nu1){
  data[[name]] * lambda1 + nu1
}
nn$data[[1]][[1]]
map(nn$data[[1]], ~customfunc(data=.,name=1, lambda1=1, nu1=nu1))
k = nn %>% mutate(
  x1 = map(data, ~customfunc(data=.,name=1, lambda1=1, nu1=nu1)),
  x2 = map(data, ~customfunc(data=.,name=1, lambda1=lambda1, nu1=nu2)),
  x3 = map(data, ~customfunc(data=.,name=1, lambda1=lambda2, nu1=nu3)),
  x4 = map(data, ~customfunc(data=.,name=2, lambda1=lambda3, nu1=nu4)),
  x5 = map(data, ~customfunc(data=.,name=2, lambda1=lambda4, nu1=nu5)),
  x6 = map(data, ~customfunc(data=.,name=2, lambda1=lambda5, nu1=nu6))) 
k %>% select(data:x6) 



%>% 
  unnest
%>% 
  head

%>% 
  unnest



y %>% mutate(stuff = map(data, ~xts(order.by = .x$date_time)))

xts
  map(data, ~customfunc(.x, lambda1=lambda1, nu1=nu1))

nn$data
variables
1:500 %>% map_dfc(.x, function(x) variables$eta1[variables$i==x] + params$nu1)



?map
\\\\%>% 
  group_by(i) %>% 
  mutate(x1 = nu$nu[1] + eta1,
         x2 = nu$nu[2] + lambda$lambda[1]*eta1,
         x3 = nu$nu[3] + lambda$lambda[2]*eta1,
         x4 = nu$nu[4] + lambda$lambda[3]*eta2,
         x5 = nu$nu[5] + lambda$lambda[4]*eta2,
         x6 = nu$nu[6] + lambda$lambda[5]*eta2)   





summarized_dataset = variables %>% summarize_at(vars(eta1:x6), mean, na.rm=T)
sample_dataset = variables %>% 
  group_by(i) %>%
  do(sample_n(.,1))

flexplot(eta1~eta2, data=sample_dataset)
flexplot(x1~x2, data=sample_dataset)
  



lvs = data.frame(blavInspect(bayes_fit_crossloadings_correct, 'lvmeans'))
names(lvs) = c("f1", "f2")
flexplot(f2~f1, data=lvs)
cor(lvs)
summary(bayes_fit_crossloadings_correct, standardized=TRUE)
medians = blavInspect(bayes_fit_crossloadings_correct, 'postmedian')
lvs$x1 = lvs$f1*1 + 9.965
lvs$x2 = lvs$f1*2.23 + 19.899
lvs$x3 = lvs$f1*3.361 + 30.119
head(lvs)
flexplot(x1~x2, data=crossloadings_small) +
  geom_line(data=lvs, aes(x2, x1))


str(mm)
mm = blavInspect(bayes_fit_crossloadings_correct, 'mcmc')



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
