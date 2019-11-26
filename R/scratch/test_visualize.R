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
coef(fit.lavaan)
coef(fit.lavaan2)


