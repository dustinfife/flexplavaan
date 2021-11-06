# this function takes an existing plot then changes the names of the plots
#plot_scatter_matrix(fit_bollen$lavaan, subset=1:3, plot="all") %>% modify_model_names(c("a", "b"))
#plot_scatter_matrix(fit_twofactor$lavaan, fit_twofactor_2$lavaan, subset=c("x1", "x2", "x3"), plot="all") %>%
# modify_model_names(c("a", "b"))
modify_model_names = function(p, model_names=c("a", "b")) {
  if(class(p)[1] != "gg") stop("This function only takes a plot as input.")
  return(p + scale_color_hue(labels = model_names))
}

# p = latent_plot(fit_bollen, formula = Eta2 ~ Eta1)[[1]]
# formula = Eta1~Eta2
# this function modifies a plot to use a new flexplot formula
# latent_plot(fit_bollen) %>% pluck(1) %>% modify_formula(Eta2~Eta1)
modify_formula = function(p, formula) {
  if(class(p)[1] != "gg")      stop("This function only takes a plot as input.")
  if(class(formula)[1] != "formula") stop("This function requires a valid formula.")
  
  # check whether variables in formula are in dataset
  check_formula_in_data(p$data, formula)
  
  # plot that thang
  data = p$data
  p = flexplot(f, data, se=F, ghost.line="red", sample=0, method=method,...) + 
    geom_point() +
    alpha_default[1] +
    alpha_default[2]
  p = switch_layer_orders(p)
  return(p)
}