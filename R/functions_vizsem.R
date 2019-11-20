    ### this function adds values to be predicted in JAGs
prediction_matrix = function(data, jagsdat){
  ## extract names of variables
  all.vars = names(jagsdat)
  keep.vars = all.vars[1:(which(all.vars == "g")-1)]
  
  new_data = keep.vars %>% map(sequence_grid, data=data) %>% expand.grid
  names(new_data) = keep.vars
  
  old.length = length(jagsdat[[keep.vars[1]]])
  
  for (i in 1:ncol(new_data)){
    jagsdat[[paste0(keep.vars[i], "_pred")]] = new_data[,keep.vars[i]]
  }
  jagsdat[["N.sim"]] = nrow(new_data)
  #jagsdat[["g"]] = rep(1, times=length(jagsdat[[keep.vars[i]]]))
  #jagsdat[["predictions"]] = c(jagsdat[["predictions"]], rep(1, times=nrow(new_data)))
  jagsdat
}

sequence_grid = function(name, data, out=10){
  minim = min(data[,name], na.rm=T)
  maxim = max(data[,name], na.rm=T)
  seq(from=minim, to=maxim, length.out=out)
}


#### create a function that generates predictions
generate_predictions = function(object, variable, data=NULL){
  if (class(object)=="runjags"){
    lav.data = data
    cnames = names(data)
  } else {
    lav.data = lavInspect(object, "data") 
    cnames = dimnames(lav.data)[[2]]
  }
  mins = apply(lav.data, 2, min)
  max = apply(lav.data, 2, max)
  means = apply(lav.data, 2, mean)
  sds = apply(lav.data, 2, sd)
  
  cnames = dimnames(lav.data)[[2]]
  mean.replace = cnames[cnames!=variable]
  new.data = data.frame(matrix(means, nrow=20, ncol=length(means), byrow=T))
  names(new.data) = cnames
  ## lavaan requires variability for predict to work
  new.data = new.data + matrix(rnorm(20*length(means), 0, .001*sds), nrow=20, byrow=T, ncol=length(means))
  new.data[,variable] = seq(from=mins[[variable]], to=max[[variable]], length.out=nrow(new.data))
  
  new.data[,"latent"] = lavPredict(object, newdata = new.data, method="Bartlett")
  return(new.data)
}
#variable = x
diagnostic_plots = function(x,y,latent, data, object, plot="model"){
  
  data$residual = data[,y] - data[,latent]
  predicted = generate_predictions(object, x, data=data)
  predicted$f = predicted$latent
  predicted$latent = fifer::rescale(predicted[,"latent"],new.mean = mean(data[,y]), new.sd = sd(data[,y]))
  
      #### trace plot needs to subtract out the effect of everything but the two variables we're looking at
  trace.plot = ggplot(data=data, aes_string(x,y))+
    geom_point() +
    geom_line(data=predicted, aes_string(x, "latent")) +
    theme_bw() +
    labs(y=y, x=x)
  
  model.plot = ggplot(data=data, aes_string(x,latent))+
    geom_point() +
    geom_line(data=predicted, aes_string(x, "f")) +
    theme_bw() +
    labs(y="Latent Variable", x=x)  
  
  residual.plot = ggplot(data=data, aes_string(x,"residual"))+
    geom_point() +
    geom_smooth() +
    theme_bw() +
    labs(y=paste0(y, " | latent"), x=x)
  
  if (plot=="model"){
    print(model.plot)
  } else if (plot == "trace"){
    print(trace.plot)
  } else {  
    print(residual.plot)
  }
}