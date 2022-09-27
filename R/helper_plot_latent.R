# this will automatically create a flexplot function based on beta matrix
beta_to_flexplot = function(fitted, return_dvs=FALSE) {
  
  data = get_lav_data(fitted)
  
  # add fake column for latents 
  # (because make_flexplot_formula checks for the variable names in the dataset)
  latents = lavNames(fitted, type="lv")
  data[,latents] = 1
  
  # beta matrix isn't just latent variables
  # if observed are endogenous, they will be there too
  # so I can't just use lavNames to get that
  
  # lavaan matrices are lambda, theta, psi, and beta.
  # beta specifies which variables are endogenous.
  # beta matrix will be in the 4th fitted@Model@dimNames slot
  if (length(fitted@Model@dimNames)>3) {
    end_names = get_endogenous_names(fitted)
  
    # get the beta matrix (which is the path coefficients between latent variables)
    beta_matrix = fitted@Model@GLIST$beta
    
    # dvs will identify which endogenous variables have predictors
    dvs = which(rowSums(beta_matrix)>0)
    
    model_formulas = dvs %>% purrr::map(~
                                          flexplot:::make_flexplot_formula(
                                            predictors = end_names[get_dv_iv(.x, beta_matrix)],
                                            outcome = end_names[.x], 
                                            data=data
                                          )
                                        )
    if (return_dvs) return(dvs)
    return(model_formulas)
    
    
  } 
  
  # if they don't have endogenous variables, just feed it to flexplot  
  dvs = lavaan::lavNames(fitted, type="lv")
  if (return_dvs) return(1:length(dvs))
  head(data)
  model_formulas = flexplot:::make_flexplot_formula(predictors = dvs[-1],
                                                    outcome = dvs[1], 
                                                    data=data)
  return(model_formulas)
}


get_endogenous_names = function(fitted){
  # lavaan matrices are lambda, theta, psi, and beta.
  # beta specifies which variables are endogenous.
  # if there's no beta, all relationships are correlational
  if (length(fitted@Model@dimNames)>3) {
    return(fitted@Model@dimNames[[4]][[1]])
  }
  return(fitted@Model@dimNames[[3]][[1]])
}


# this function identifies which variables are endogenous using a beta matrix
get_dv_iv = function(i, beta_matrix){
  which(abs(beta_matrix[i,])>0)
}
