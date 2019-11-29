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

require(lavaan)
fit.lavaan = cfa(model_correct, data=crossloadings_small)
fit.lavaan2 = cfa(model_missspecified, data=crossloadings_small)
#viz_diagnostics(data = crossloadings_small, mapping = aes(x1,y1), fit.lavaan = fit.lavaan, fit.lavaan2 = fit.lavaan2, plot="trace")
visualize(fit.lavaan, fit.lavaan2)
viz_diagnostics(data = crossloadings_small, 
                mapping = aes(x2,x2), fit.lavaan = fit.lavaan, fit.lavaan2 = fit.lavaan2, plot="histogram")
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
