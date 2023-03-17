
# taken from OpenMx
vechs = function(x) {
  return(x[lower.tri(x, diag=FALSE)])
}


reverse_rank = function(x) {
  rank(x*-1 )
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

empirical_reliability = function(latent, observed, loess=FALSE){
  if (!loess) return(cor(latent, observed)^2)
  predicted = cbind(latent,observed) %>% 
    data.frame %>% setNames(c("latent", "observed"))  
  predicted = loess(formula = observed~latent, data=predicted, degree=2)$fitted
  ss_reg = sum((predicted - mean(unlist(observed), na.rm=T))^2, na.rm=T)
  ss_tot = sum((observed-mean(unlist(observed), na.rm=T))^2, na.rm=T)
  return(ss_reg/ss_tot)
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

residualize.lowess = function(x,y, data, return.fitted){
  f = flexplot::make.formula(y, x)
  lfit = loess(f, newdata, degree=2)
  
  # create a functional version of the lowess fit
  lfun = approxfun(lfit)
  fitted = lfun(x)
  resid = y-fitted
  if(return.fitted) return(fitted)
  return(resid)
}