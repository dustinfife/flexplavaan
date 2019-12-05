
nonlinear_prediction = function(x,y,latent){
  pred.x = cbind(latent[,1, drop=FALSE],x) %>% 
    data.frame %>% 
    setNames(c("latent", "x")) %>% 
    loess(formula = x~latent, data=., degree=2) 
  pred.x = pred.x$fitted
  pred.y = cbind(latent[,2, drop=FALSE],y) %>% 
    data.frame %>% 
    setNames(c("latent", "y")) %>% 
    loess(formula = y~latent, data=., degree=2)
  pred.y = pred.y$fitted
  #browser()
  rxx = empirical_reliability(latent[,1], x, loess=TRUE)
  ryy = empirical_reliability(latent[,2], y, loess=TRUE)

  ## correct for reliability
  #pred.x = mean(pred.x) + sqrt(rxx*ryy)*(pred.x-mean(pred.x))
  pred.y = mean(pred.y) + sqrt(rxx*ryy)*(pred.y-mean(pred.y))
  
  list(x=pred.x, y=pred.y)
}

visualize_nonlinear = function(x,y,latent, plot){

  x.names = names(x)
  y.names = names(y)
  data = data.frame(x,y)
  names(data) = c(x.names, y.names)
  newpred = nonlinear_prediction(x, y, latent) 
  newpred = data.frame(newpred)
  form = flexplot::make.formula(y.names, x.names)
  if (plot=="trace"){
    flexplot(form, data=data) +
      geom_line(data=newpred, aes(x, y), col="red", size=1.5)    
  } else if (plot == "disturbance"){
    data$residuals = unlist(y - newpred$y)
    flexplot(flexplot::make.formula("residuals",x.names), data=data) +
      geom_hline(yintercept = 0, col="red", size=1.5)
  }
  
}

empirical_reliability = function(latent, observed, loess=FALSE){
  if (loess) {
    predicted = cbind(latent,observed) %>% 
      data.frame %>% setNames(c("latent", "observed"))  
    predicted = loess(formula = observed~latent, data=predicted, degree=2)$fitted
    ss_reg = sum((predicted - mean(unlist(observed)))^2)
    ss_tot = sum((observed-mean(unlist(observed)))^2)
    return(ss_reg/ss_tot)
  } else {
    return(cor(latent, observed)^2)
  }
}


### this function takes a binned x/y and computes the average of the latent
### variable within a specific bin level
joint_bin = function(i,xbin,ybin,latent){
  
  rows = which(xbin==levels(xbin)[i] & ybin==levels(ybin)[i])
  if (sum(rows)>0){
    return(mean(latent[rows]))
  } else {
    return(NA)
  }  
}

ss = function(x){
  sum((x-mean(x))^2)
}
logistic = function(x, mx, x0, x1, y0, y1, rxx, ryy){
  x[x>mx] = mx-.1 
  slope = -1*(y1/x1)*sqrt(rxx*ryy)
  mx/(1 + exp(slope*(x-x0) - y1*y0))
}

inv.logistic = function(x, mx, x0, x1, y0, y1, rxx, ryy){
  x[x>mx] = mx-.1 
  slope = (y1/x1)*sqrt(rxx*ryy)
  y0 - slope*(log((mx/x)-1) + x0*y1)
}

linear = function(x, x0, x1, y0, y1, rxx, ryy, mx=NULL){
  y0 + (y1/x1)*sqrt(rxx*ryy)*(x-x0)
}

extract_xy_mapping = function(mapping, invert.map, data, observed, latent=NULL){
  variables = c(dplyr::as_label(mapping$x), dplyr::as_label(mapping$y))
  
  ### extract name of variable in aes
  if (invert.map){
    x = variables[2]
    y = variables[1]   
  } else {
    y = variables[2]
    x = variables[1]
  }
  #browser()
  if (!all(variables %in% names(data))){
    problem.vars = which(!(variables %in% names(data)))
    var = ifelse(length(problem.vars>1), 
                 paste0("variables ", variables[problem.vars], " are"), 
                 paste0("variable ", variables[problem.vars], " is")) 
    msg = paste0("The ", var, " not in your actual dataset.")
    stop(msg)
  }
  
  if (!all(variables %in% observed)){
    problem.vars = which(!(variables %in% observed))
    var = ifelse(length(problem.vars>1), 
                 paste0("variables ", variables[problem.vars], " are"), 
                 paste0("variable ", variables[problem.vars], " is"))
    msg = paste0("The ", var, " not in your actual dataset.")
    stop(msg)
  }  
  
  if (!is.null(latent)){
    latent = latent[which(names(data) == x | names(data) == y)]
    list(x=x, latent=latent, y=y)
  } else {
    list(x=x, y=y)
  }
  
  
}


residualize.lowess = function(x,y, data, return.fitted){
  f = flexplot::make.formula(y, x)
  lfit = loess(f, newdata, degree=2)

  # create a functional version of the lowess fit
  lfun = approxfun(lfit)
  fitted = lfun(x)
  resid = y-fitted
  if(return.fitted) {
    return(fitted)
  } else {
    return(resid)
  }
}