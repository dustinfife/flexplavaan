#### create a function that generates predictions
generate_predictions = function(object, variable){
  lav.data = lavInspect(object, "data") 
  mins = apply(lav.data, 2, min)
  max = apply(lav.data, 2, max)
  means = apply(lav.data, 2, mean)
  sds = apply(lav.data, 2, sd)
  
  cnames = dimnames(lav.data)[[2]]
  mean.replace = cnames[cnames!=variable]
  new.data = data.frame(matrix(means, nrow=20, ncol=length(means), byrow=T))
  names(new.data) = cnames
  ## lavaan requires variability for the predict to work
  new.data = new.data + matrix(rnorm(20*length(means), 0, .001*sds), nrow=20, byrow=T, ncol=length(means))
  
  new.data[,variable] = seq(from=mins[[variable]], to=max[[variable]], length.out=nrow(new.data))
  new.data[,"latent"] = lavPredict(object, newdata = new.data)
  return(new.data)
}

diagnostic_plots = function(x,y,latent, data, object, plot="model"){
  
  data$residual = data[,y] - data[,latent]
  #data$predicted = fifer::rescale(data[,latent],new.mean = mean(data[,y]), new.sd = sd(data[,y]))
  predicted = generate_predictions(object, x)
  predicted$f = predicted$latent
  predicted$latent = fifer::rescale(predicted[,"latent"],new.mean = mean(data[,y]), new.sd = sd(data[,y]))
  
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