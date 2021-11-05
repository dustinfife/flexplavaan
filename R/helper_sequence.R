#data = mugglevwizard; condition.vars = c("wingardium", "strange")
create_new_data = function(data, condition.vars=NULL){
  variables = names(data)
  if (is.null(condition.vars)){
    vars = variables
  }
  sequences = variables %in% condition.vars
  
  if (sum(sequences)>0){
    new_data = 1:sum(sequences) %>% 
      purrr::map(~create_sequence(condition.vars[.x], data, sequence = TRUE)) %>%  
      data.frame %>%
      setNames(condition.vars) %>% 
      expand.grid
  }
  
  fixed_data = 1:sum(!sequences) %>% 
    purrr::map(~create_sequence(variables[!sequences][.x], data, sequence = FALSE)) %>%  
    data.frame %>%
    setNames(variables[!sequences]) 
  
  if (sum(sequences)>0){
    new_data = data.frame(cbind(new_data, fixed_data))  
  } else {
    new_data = fixed_data
  }
  
  return(new_data)
}

sequence = function(x, length=20){
  seq(from=min(x), to=max(x), length.out = length)
}

#create_sequence("wingardium", mugglevwizard, sequence=TRUE, round=TRUE)
#create_sequence("wingardium", mugglevwizard, sequence=TRUE, round=FALSE)
#create_sequence("wingardium", mugglevwizard, sequence=FALSE, round=FALSE)
#create_sequence("wingardium", mugglevwizard, sequence=FALSE, round=TRUE)
#create_sequence("mugglevwizard", mugglevwizard, sequence=FALSE)
#create_sequence("mugglevwizard", mugglevwizard, sequence=TRUE)
create_sequence = function(name, data, sequence=FALSE, length = 20, round=FALSE){
  if (sequence == FALSE){
    if (is.numeric(data[,name])){
      k = (mean(data[,name]) + rnorm(length, 0, sd(data[,name])*.001))  
      if (round) k = round(k)
    } else {
      k = data[,name][1]
    }
  } else {
    if (is.numeric(data[,name])){
      k = seq(from=min(data[,name], na.rm = T), to=max(data[,name], na.rm = T), length.out=length)
      if (round) k = round(k)
    } else {
      k = levels(factor(data[,name]))
    }
  }
  return(k)
}
