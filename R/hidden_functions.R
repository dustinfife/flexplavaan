empirical_reliability = function(latent, observed, b0, b1, mx=NULL){
  if (!is.null(mx)) {
    
    predicted = mx/(1 + exp(-1*b1*(latent - b0)))
    ss_reg = sum((predicted - mean(observed))^2)
    ss_tot = sum((observed-mean(observed))^2)
    return(ss_reg/ss_tot)
  } else {
    return(cor(latent, observed)^2)
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