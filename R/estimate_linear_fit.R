estimate_linear_fit = function(fit.lavaan, x, y, data){
  
  if (class(fit.lavaan)=="matrix"){
    
    rel.x = var(nonlinear$V1)-fit.lavaan["theta[1,1,1]","Mean"]^2
    rel.y = 1- (fit.lavaan["theta[2,2,1]","Mean"]^2/var(nonlinear$V2))
  
    lvs = startsWith(dimnames(fit.lavaan)[[1]], "eta")
    lvs = fit.lavaan[lvs,"Mean"] %>% as.data.frame %>% setNames("f")
    
    newdata = cbind(data, lvs) %>% 
      data.frame %>% 
      mutate(!!"f" := rescale(f, mean(!!(as.name(y))), sd(!!(as.name(y)))))
    
    x_new = quantile(newdata[,x], probs = seq(from=0, to=1, length.out=80)) %>% as.numeric
    y_new = quantile(newdata[,"f"], probs = seq(from=0, to=1, length.out=80)) %>% as.numeric
    y_new = mean(y_new) + sqrt(rel.x*rel.y) * (y_new-mean(y_new))
    y_new = y_new - 3
    # transform f to reduce it's slope by reliability
    fnew = newdata[["f"]]
    newdata[["f"]] = mean(fnew) + sqrt(rel.x*rel.y) * (fnew-mean(fnew))    
    residuals = newdata[[y]] - newdata[["f"]]
    
    flexplot(V2~V1, data=data) +
      geom_line(data=data.frame(x_new=x_new, y_new=y_new), aes(x_new, y_new), col="red")
    
  }  
  
  
  
  if (class(fit.lavaan)=="blavaan"){
    
    lambda = inspect(fit.lavaan,what="std")$lambda
    rel.x = sum(lambda[x,])^2  ### DO YOU SUM FACTOR LOADINGS TO GET RELIABILITY?
    rel.y = sum(lambda[y,])^2
    factors = names(which(abs(lambda[y,])>0))
    
    lvs = data.frame(blavInspect(fit.lavaan, 'lvmeans')) %>% 
      setNames(dimnames(lambda)[[2]]) %>% 
      mutate(!!"f" := !!(as.name(factors))) %>%
      select("f")
    
    newdata = cbind(data, lvs) %>% 
      data.frame %>% 
      mutate(!!"f" := rescale(f, mean(!!(as.name(y))), sd(!!(as.name(y)))))
    
    x_new = quantile(newdata[,x], probs = seq(from=0, to=1, length.out=30)) %>% as.numeric
    y_new = quantile(newdata[,"f"], probs = seq(from=0, to=1, length.out=30)) %>% as.numeric
    y_new = mean(y_new) + sqrt(rel.x*rel.y) * (y_new-mean(y_new))
    
    # transform f to reduce it's slope by reliability
    fnew = newdata[["f"]]
    newdata[["f"]] = mean(fnew) + sqrt(rel.x*rel.y) * (fnew-mean(fnew))    
    residuals = newdata[[y]] - newdata[["f"]]
  } else {
  
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
  }

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
