get_legend = function(object2) {
  if (is.null(object2)) return(NULL) else return(c(1,2))
}

get_names = function(model) {
  obs_names = lavNames(model)
  latent_names = lavNames(model, type="lv")
  list(obs_names, latent_names)
}

check_models = function(model, model2=NULL){
  if (is.null(model2)) return(NULL)
  # make sure all variables in MODEL 1 are in model 2 (no need to do vice versa)
  if (!(all(lavNames(model) %in% lavNames(model2)))) stop("You must have the same observed variables in both models")
  if (!(all(lavNames(model, type="lv") %in% lavNames(model2, type="lv")))) stop("You must have the same latent variables in both models")  
  return(NULL)
}

get_and_check_names = function(model_names, object, object2) {
  if (!is.null(model_names)) return(model_names)
  if (is.null(model_names) & is.null(object2)) return(c("Model-Implied", "Data-Implied"))
  a = deparse(substitute(object,sys.frame(sys.nframe()-1)))
  b = deparse(substitute(object2,sys.frame(sys.nframe()-1)))
  if (!is.null(object2) & is.null(model_names)) return(c(a,b))
  return(model_names)
}

block_model_residuals = function(fitted) {
  
  obs_names = lavNames(fitted)
  residual_correlations = residuals(fitted, type="cor")$cov 
  
  #Cluster based on structural equivalence
  eq<-sna::equiv.clust(residual_correlations, equiv.fun = sna::sedist)
  block = sna::blockmodel(residual_correlations, eq, k = length(obs_names), 
                          mode="graph")
  column_order = as.numeric(sapply(block$plabels, function(x) which(obs_names==x)))
  return(column_order)
}

reverse_rank = function(x) {
  rank(x*-1 )
}

# taken from OpenMx
vechs = function(x) {
  return(x[lower.tri(x, diag=FALSE)])
}

# taken from OpenMx
vech2full = function (x) 
{
  if (is.matrix(x)) {
    if (nrow(x) > 1 && ncol(x) > 1) {
      stop("Input to the full vech2full must be a (1 x n) or (n x 1) matrix.")
    }
    dimension <- max(dim(x))
  }
  else if (is.vector(x)) {
    dimension <- length(x)
  }
  else {
    stop("Input to the function vech2full must be either a matrix or a vector.")
  }
  k <- sqrt(2 * dimension + 0.25) - 0.5
  ret <- matrix(0, nrow = k, ncol = k)
  if (nrow(ret) != k) {
    stop("Incorrect number of elements in vector to construct a matrix from a half-vectorization.")
  }
  ret[lower.tri(ret, diag = TRUE)] <- as.vector(x)
  ret[upper.tri(ret)] <- t(ret)[upper.tri(ret)]
  return(ret)
}

#varnames = letters[1:5]
random_var_name = function(size=5) {
  letters[1:26] %>% sample(size=size, replace=T) %>% paste0(collapse="")
}




random_var_name_check = function(varnames) {
  newname = random_var_name(5)
  while ((newname %in% varnames)){
    newname = random_var_name(5)
  }
  newname
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

# get the factors associated with this i variable
find_latents_for_observed = function(i, fitted) {
  row = fitted@Model@GLIST$lambda[i,]
  latent = lavNames(fitted, type="lv")[round(row, digits=4)!=0]
  return(latent)
}

inv.logistic = function(x, mx, x0, x1, y0, y1, rxx, ryy){
  x[x>mx] = mx-.1 
  slope = (y1/x1)*sqrt(rxx*ryy)
  y0 - slope*(log((mx/x)-1) + x0*y1)
}

linear = function(x, x0, x1, y0, y1, rxx, ryy, mx=NULL){
  y0 + (y1/x1)*sqrt(rxx*ryy)*(x-x0)
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
  
  rxx = empirical_reliability(latent[,1], x, loess=TRUE)
  ryy = empirical_reliability(latent[,2], y, loess=TRUE)
  
  ## correct for reliability
  #pred.x = mean(pred.x) + sqrt(rxx*ryy)*(pred.x-mean(pred.x))
  pred.y = mean(pred.y) + sqrt(rxx*ryy)*(pred.y-mean(pred.y))
  
  list(x=pred.x, y=pred.y)
}

# expect_equal(class(set_model_class(a=1, plot="hello")), "hello")
set_model_class = function(..., plot) {
  x = list(...)
  structure(x, class=plot)
}


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


# x = "flying"
# y = "darkarts"
# latents = c(2,2); fit.mcmc = fit.lavaan
estimate_nonlinear_fit = function(latentscores, x, y, data, which.latent=c(1,1)){
  
  #browser()
  latent.names = unique(names(latentscores)[(which.latent+1)])
  newdata = cbind(data, latentscores) %>% 
    data.frame 
  
  ### use loess to fit (to estimate reliability)
  fit_y = flexplot::make.formula(y,latent.names[which.latent[2]]) %>% loess(newdata, degree=2)
  fit_x = flexplot::make.formula(x,latent.names[which.latent[1]]) %>% loess(newdata, degree=2)
  
  rel.y = var(fit_y$fitted-mean(newdata[,y]))/var(newdata[,y])
  rel.x = var(fit_x$fitted-mean(newdata[,x]))/var(newdata[,x])
  
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
