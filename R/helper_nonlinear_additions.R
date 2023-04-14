



extract_variables_from_nonlinear_syntax = function(model_addition) {
  # get rid of paragraph breaks
  model_addition = gsub("\n", "", model_addition)
  dv = gsub("(.*?)=.*", "\\1", model_addition) %>% trimws
  
  
  link_and_variable = gsub(".*I\\([ ]?(.+)[ ]?\\).*", "\\1", model_addition) %>% trimws
  iv = gsub("\\b([a-zA-Z_\\.]+)[^a-zA-Z_\\.]*$", "\\1", link_and_variable)
  list(dv = dv, link_and_variable = link_and_variable, iv = iv)
  
}

# find the maximum lambda value
maximum_lambda = function(all_elements) {
  last_lambda_string = grep("lambda", all_elements$B, value=T) %>% tail(n=1)
  # extract the number
  last_number = gsub("mu\\[i,([0-9]+).*", "\\1", last_lambda_string) %>% trimws() %>% as.numeric
  # increment the number
  new_number = last_number + 1
  return(new_number)
}
