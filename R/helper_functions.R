# this function takes the input from a formula and ensures all
# elements in the formula are in the dataset
check_formula_in_data = function(data, formula) {
  variables = all.vars(formula, unique=FALSE)
  all_there = all(variables %in% names(data))
  if (!all_there) stop("Some of the variables in your formula are not in your dataset.")
  return(NULL)
}

# this identifies whether a second object is there. if not, it returns a NULL legend
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

get_and_check_names = function(model_names=NULL, object, object2) {
  
  if (!is.null(model_names)) return(model_names)
  if (is.null(model_names) & is.null(object2)) return(c("Model-Implied", "Data-Implied"))
  a = deparse(substitute(object,sys.frame(sys.nframe()-3)))
  b = deparse(substitute(object2,sys.frame(sys.nframe()-3)))
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



# get the factors associated with this i variable
find_latents_for_observed = function(i, fitted) {
  row = fitted@Model@GLIST$lambda[i,]
  latent = lavNames(fitted, type="lv")[round(row, digits=4)!=0]
  return(latent)
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





# x = "flying"
# y = "darkarts"
# latents = c(2,2); fit.mcmc = fit.lavaan


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
