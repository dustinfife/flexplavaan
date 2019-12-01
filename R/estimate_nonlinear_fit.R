# x = "flying"
# y = "darkarts"
# latents = c(2,2); fit.mcmc = fit.lavaan
estimate_nonlinear_fit = function(fit.mcmc, x, y, data, latents){
  
  latent.names = names(fit.mcmc)[1+latents]
  newdata = cbind(data, fit.mcmc) %>% 
    data.frame #%>% 
    #mutate(!!latent.names[2] := fifer::rescale(!!as.name(latent.names[2]), mean(!!(as.name(y))), sd(!!(as.name(y))))) 
  ### rescale latent variable to mean of y so we can compute residuals later
    #mutate(!!latent.names[1] := fifer::rescale(!!as.name(latent.names[1]), mean(!!(as.name(x))), sd(!!(as.name(x))))) %>% 

  ### use loess to fit (to estimate reliability)
  fit_y = flexplot::make.formula(y,latent.names[2]) %>% loess(newdata, degree=2)
  fit_x = flexplot::make.formula(x,latent.names[1]) %>% loess(newdata, degree=2)
  
  rel.y = var(fit_y$fitted-mean(newdata[,y]))/var(newdata[,y])
  rel.x = var(fit_x$fitted-mean(newdata[,x]))/var(newdata[,x])
  
  # plot(newdata[[latent.names[2]]], newdata[[y]])
  # nd = data.frame(seq(min(newdata[[latent.names[2]]]), max(newdata[[latent.names[2]]]), length.out=20))
  # names(nd) = latent.names[2]
  # lines(nd[[latent.names[2]]],predict(fit_y, newdata=nd))
  # 
  # plot(newdata[[latent.names[1]]], newdata[[x]])
  # nd = data.frame(seq(min(newdata[[latent.names[1]]]), max(newdata[[latent.names[1]]]), length.out=20))
  # names(nd) = latent.names[1]
  # lines(nd[[latent.names[1]]],predict(fit_x, newdata=nd))  
  
  ### generate line for prediction (x is always the latent variable, because that's how it was generated)
  y_new = quantile(newdata[,y], probs = seq(from=0, to=1, length.out=80)) %>% as.numeric
  x_new = quantile(newdata[,"magic_skills"], probs = seq(from=0, to=1, length.out=80)) %>% as.numeric
  x_new = fifer::rescale(x_new, new.mean = mean(newdata[,x]), new.sd = sd(newdata[,x]))
  x_new2 = mean(x_new) + sqrt(rel.x*rel.y)* (x_new-mean(x_new))
  
  #### figure out the mean of the latent for every value of x, then transform
  quants = quantile(newdata[,x], seq(0,1,length.out=30))
  newdata$breaks = cut(newdata[,x], breaks=quants, include.lowest=T)
  predicted = newdata %>% group_by(breaks) %>% 
    summarize(factors = mean(!!(as.name(latent.names[1])))) %>% 
    mutate(breaks = round(quants)[-1]) %>% 
    mutate(factors = fifer::rescale(factors, mean((newdata[,y])), sd(newdata[,y]))) %>% 
    mutate(factors2 = mean(factors) + sqrt(rel.x*rel.y) * (factors-mean(factors)))
  
  form = flexplot::make.formula(y, x)
  flexplot(form, data=data) +
    geom_line(data=predicted, aes(breaks, factors), col="red") +
    geom_line(data=predicted, aes(breaks, factors2), col="blue") 

}

# plot(newdata$flying, newdata$magic_skills)
# plot(newdata$flying, newdata$darkarts)

# ggplot(newdata, aes(flying, darkarts, color=factor2)) +
#   geom_point()
# 
# flexplot(flying~darkarts, newdata)
# flexplot(flying~factor2, newdata)
# 
# flexplot(darkarts~flying, newdata)
# flexplot(darkarts~factor2, newdata)  