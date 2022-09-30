#expect_equal(invert_aes_mapping(aes(x,y), TRUE), c("y", "x"))
#expect_equal(invert_aes_mapping(aes(x,y), FALSE), c("x", "y"))
invert_aes_mapping = function(mapping, invert.map) {
  variables = c(dplyr::as_label(mapping$x), dplyr::as_label(mapping$y))
  ### extract name of variable in aes
  if (!invert.map) return(variables)
  return(variables[c(2,1)])
}



viz_diagnostics_error_check = function(variables, fitted) {
  
  observed = lavNames(fitted)
  data = get_lav_data(fitted)
  # when doing histograms, one of the variables is "NULL", so remove those cases for this check
  if ("NULL" %in% variables) variables = variables[variables != "NULL"]
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
  return(NULL)
}

# new_data is the fitted line (there will be one or more)
viz_diagnostics_get_data = function(fit.lavaan, fit.lavaan2=NULL, variables){

  x = variables[1]
  y = variables[2]
  data = get_lav_data(fit.lavaan)
  
  estimated_fits = estimate_linear_fit(fit.lavaan, x=x, y=y, data)
  new_data = data.frame(x=estimated_fits$x_new, y=estimated_fits$y_new)
  names(new_data) = c(x, y)
  data[,"residuals"] = estimated_fits$residuals

  if (!is.null(fit.lavaan2)) {
    estimated_fits = estimate_linear_fit(fit.lavaan2, x, y, data)
    # come up with fake name for second variable
    y2_name = random_var_name_check(names(new_data))
    resid_name = random_var_name_check(names(new_data))
    new_data[[y2_name]] = estimated_fits$y_new
    data[[resid_name]] = estimated_fits$residuals      
  } else {
    y2_name = NULL
    resid_name = NULL
  }
  return(list(data=data, new_data=new_data, y2_name=y2_name, resid_name = resid_name))
}

extract_subset = function(object, subset) {
  names = lavNames(object)
  
  if (length(subset)>length(names)) stop("It looks like you're trying to extract more variables than you have.")
  if (is.numeric(subset)) {
    if ((max(subset) > length(names))) stop("It looks like you're trying to extract variables that don't exist.")
    return(names[subset])
  }
  
  if (!all(subset %in% names)) {
    badvar = subset[!(subset %in% names)]
    stop(paste0("One of the variables you're looking for (", paste0(badvar), ") is not in your model"))
  }
 
  names(names) = names
  return(as.character(names[subset]))
}

variable_not_in_here = function(tocheck, available) {
  
}




# viz_diagnostics_get_data = function(fit.lavaan, fit.lavaan2, variables){
#   
#   x = variables[1]
#   y = variables[2]
#   data = get_lav_data(fit.lavaan)
#   
#   estimated_fits = estimate_linear_fit(fit.lavaan, x=x, y=y, data)
#   data[,"residuals"] = estimated_fits$residuals
#   
#   if (is.null(fit.lavaan2)) return(data)
#   
#   estimated_fits = estimate_linear_fit(fit.lavaan2, x, y, data)
#   # come up with fake name for second variable
#   y2_name = random_var_name_check(names(new_data))
#   resid_name = random_var_name_check(names(new_data))
#   data[[resid_name]] = estimated_fits$residuals      
#   return(list(data=data, new_data=new_data))
# }






#