estimate_linear_fit = function(fit.lavaan, x, y, data){
  
  ### reliability is needed for the correction factor
  rel.x = sum(inspect(fit.lavaan,what="std")$lambda[x,])^2  ### DO YOU SUM FACTOR LOADINGS TO GET RELIABILITY?
  rel.y = sum(inspect(fit.lavaan,what="std")$lambda[y,])^2
  
  ### compute model-implied slope between the two
  predicted_data = lavPredict(fit.lavaan, type="ov")
  estimated.slope = coef(lm(flexplot::make.formula(y, x), predicted_data))[2]
  ### slope = sd(y)/sd(x) = maximum possible slope between the two, but multiply by f(reliability)
  corrected.slope = estimated.slope*sqrt(rel.x*rel.y) 
  corrected.intercept = mean(data[,y]) - corrected.slope * mean(data[,x])
  ### maximum possible value * sqrt(reliability product)
  
  x_new = seq(from=min(data[,x]), to=max(data[,x]), length.out=20)
  y_new = corrected.intercept + corrected.slope*x_new
  residuals = data[,y] - (corrected.intercept + corrected.slope*data[,x])

  list(x_new = x_new, y_new = y_new, residuals = residuals)
}
# 
# head(practice_large)
# data("practice_large")
# model = "
# f1 =~ x1 + x2 + x3
# f2 =~ y1 + y2 + y3
# f1 ~ f2
# "
# 
# 
# require(lavaan)
# 
# fit.lavaan = cfa(model, data=correct_large)
# estimate_linear_fit(x1~y2, fit.lavaan)
# estimate_linear_fit(x1~x2, fit.lavaan)
# expect_error(estimate_linear_fit(wingardium~potions, fit.lavaan))
