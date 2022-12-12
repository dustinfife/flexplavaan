a = implied_measurement(lavfit)

# Extract the raw data from the ggplot object
residualize = function(a) {
  
  raw_data <- ggplot_build(a)
  # require(ggplot2);require(gridExtra)
  # # Modify the raw data
  # points = raw_data$data[[1]]
  # implied = raw_data$data[[2]]
  # premult_dataset = cbind(points[,c("x", "y")], implied[,c("intercept", "slope")])
  # 
  # modified_data <- premult_dataset %>% 
  #   mutate(y = y - (intercept + slope*x))
  # points$y = modified_data$y
  # raw_data$data[[1]] = points
  #   
  # recreate as a GGPLOT object
  c = ggplotify::as.ggplot(ggplot_gtable(raw_data))
  c$layers[[2]]
  return(c)
}

residualize(a) 
# remove the layers
modified_plot$layers[[2]]
str(modified_plot)
# get rid of old lines
raw_data$data[[2]] = NULL
raw_data$data[[3]] = NULL

