
estimate_linear_fit = function(fit.lavaan, x, y, data){
  
  if (class(fit.lavaan)=="matrix"){
    data(nonlinear)
    data = nonlinear
    fit.lavaan = results
    x = "V3"; y = "V2"
    rel.x = var(nonlinear$V3)-fit.lavaan["theta[1,1,1]","Mean"]^2
    rel.y = 1- (fit.lavaan["theta[2,2,1]","Mean"]^2/var(nonlinear$V2))
  
    lvs = startsWith(dimnames(fit.lavaan)[[1]], "eta")
    lvs = fit.lavaan[lvs,"Mean"] #%>% as.data.frame 
    data$f2 = lvs

    flexplot::flexplot(f2~latent, data=data)
    
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
    
    flexplot(V3~V2, data=data) +
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
  
    ### compute model-implied slope between the two
    implied.cor = lavInspect(fit.lavaan, what="cor.ov")
    implied.cov = lavInspect(fit.lavaan, what="cov.ov")
    stdev_ov = sqrt(diag(implied.cov))
    estimated.slope = implied.cor[x,y]*(stdev_ov[y]/stdev_ov[x])
    ### slope = sd(y)/sd(x) = maximum possible slope between the two, but multiply by f(reliability)
    corrected.slope = estimated.slope
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
