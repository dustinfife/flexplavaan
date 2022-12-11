# this function takes a lav object/boolean and returns a vector of ordered variables
# sorting according to residual
sort_variables = function(object, sort_plots) {
  # get the names of the variables with largest residuals
  
  residuals = return_residual_dataset(object, 0)
  variable_order = as.character(residuals$Correlation) %>%
    strsplit(":") %>%
    unlist() %>%
    unique()
  # vector_of_booleans = rep(sort_plots, times=length(lavNames(object)))
  # variable_order = ifelse(vector_of_booleans, 
  #                         block_model_residuals(object), 
  #                         1:length(lavNames(object)))
  return(variable_order)
}

gsub_piped = function(x, pattern, replacement) {
  gsub(pattern, replacement, x)
}
#testthat::snapshot_review()

# this function returns a subset of variable names
get_subset = function(varnames, subset) {
  if (is.null(subset)) return(varnames)
  if (is.numeric(subset) & any(subset>max(length(varnames)))) stop("You're trying to index a varname using a number larger than the length of varname")
  if (!all(subset %in% varnames) & !is.numeric(subset)) stop("One or more of the variables you supplied in subset is not in varnames.")
  if (is.numeric(subset)) return(varnames[subset])
  return(varnames[varnames %in% subset])
}

#return a vector of residuals from predicting observed from latent
residual_from_latents = function(i, fitted) {
  
  # get names
  observed = lavNames(fitted)[i]
  variable_name = find_latents_for_observed(i, fitted)
  
  # if there are no latens associated with this variable, just return the variable
  if (is.na(variable_name[1])) return(fitted@Data@X[[1]][,i])
  # create dataset
  variable_scores = lavPredict(fitted, type = "lv")[,variable_name]
  dataset = data.frame(cbind(variable_scores, fitted@Data@X[[1]][,i]))
  names(dataset) = c(variable_name, observed)
  
  # when there are negative variances, the latent variables predict NA. Give an error msg. 
  if (all(is.na(dataset[,variable_name]))) {
    stop("It looks like your model has negative variances. That means you won't be able to 
         plot this. Fix the model and try again.")
  }
  
  # formula/residuals
  formula_residual = flexplot::make.formula(observed, variable_name)
  residuals = residuals(lm(formula_residual, dataset))
  return(residuals)
}

# returns the lavaan object embedded in the flexplavaan object
flexplavaan_to_lavaan = function(fitted) {
  if (is.null(fitted)) return(NULL)
  if (class(fitted)=="flexplavaan") return(fitted$lavaan)
  return(fitted)
}


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

# this returns the latent/observed variable names of a lav object
get_names = function(model) {
  obs_names = lavNames(model)
  latent_names = lavNames(model, type="lv")
  list(obs_names, latent_names)
}

# this checks whether both models have the same observed/latent variables
# but this is too strict. So I changed to say check whether the latent variable 
# is in both models
check_models = function(model, model2=NULL){
  if (is.null(model2)) return(NULL)
  # make sure all variables in MODEL 1 are in model 2 (no need to do vice versa)
  if (!(all(lavNames(model) %in% lavNames(model2)))) stop("You must have the same observed variables in both models")
  if (!(all(lavNames(model, type="lv") %in% lavNames(model2, type="lv")))) stop("You must have the same latent variables in both models")  
  return(NULL)
}

# this returns different object names (for labeling scatterplot matrices)
get_and_check_names = function(model_names=NULL, object, object2) {
  if (!is.null(model_names)) return(model_names)
  if (is.null(model_names) & is.null(object2)) return(c("Model-Implied", "Data-Implied"))
  a = deparse(substitute(object,sys.frame(sys.nframe()-3)))
  b = deparse(substitute(object2,sys.frame(sys.nframe()-3)))
  if (!is.null(object2) & is.null(model_names)) return(c(a,b))
  return(model_names)
}
  
# this sorts the variabls by the rank of the residuals, then returns a vector of numbers for the order
block_model_residuals = function(fitted) {
  
  obs_names = lavNames(fitted)
  residual_correlations = residuals(fitted, type="cor")$cov 
  
  #Cluster based on average size
  c = return_residual_dataset(fitted, 0) 
  c$var1 = strsplit(as.character(c$Correlation), ":") %>% map(purrr::pluck(1)) %>% unlist
  c$var2 = strsplit(as.character(c$Correlation), ":") %>% map(purrr::pluck(2)) %>% unlist
  c$rank = 1:nrow(c)
  
  column_order = c %>% pivot_longer(var1:var2, names_to="variable", values_to="cor") %>% 
    group_by(cor) %>% 
    summarize(mean=mean(abs(rank))) %>% 
    arrange(mean) %>% 
    select(cor) %>% 
    purrr::pluck("cor") %>% 
    purrr::map_int(function(x) which(obs_names == x))
  return(column_order)
}

#varnames = letters[1:5]
random_var_name = function(size=5) {
  letters[1:26] %>% sample(size=size, replace=T) %>% paste0(collapse="")
}

# get the factors associated with this i variable
find_latents_for_observed = function(i, fitted) {
  row = fitted@Model@GLIST$lambda[i,]
  latent = lavNames(fitted, type="lv")[round(row, digits=4)!=0]
  return(latent)
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
