# this function takes an existing plot then changes the names of the plots
#plot_scatter_matrix(fit_bollen$lavaan, subset=1:3, plot="all") %>% modify_model_names(c("a", "b"))
#plot_scatter_matrix(fit_twofactor$lavaan, fit_twofactor_2$lavaan, subset=c("x1", "x2", "x3"), plot="all") %>%
# modify_model_names(c("a", "b"))
modify_model_names = function(p, model_names=c("a", "b")) {
  if(class(p)[1] != "gg") stop("This function only takes a plot as input.")
  return(p + scale_color_hue(labels = model_names))
}

# p = latent_plot(fit_bollen)
# formula = Eta1~Eta2
# this function modifies a plot to use a new flexplot formula
# latent_plot(fit_bollen) %>% modify_formula(Eta2~Eta1)
modify_formula = function(p, formula, ...) {
  if(class(p)[1] != "gg")      stop("This function only takes a plot as input.")
  if(class(formula)[1] != "formula") stop("This function requires a valid formula.")
  
  # check whether variables in formula are in dataset
  check_formula_in_data(p$data, formula)
  
  ## see if alpha is set
  list(...)
  alpha_default = return_alpha(...)
  
  # plot that thang
  data = p$data
  data = create_ci_limits(data, formula)
  
  p = plot_crosshair_plot(data, formula, alpha_default, ...)
  return(p)
}

#p = implied_measurement(small, small_mis)
modify_latent = function(p, latent, ...) {
  data = p$data
  
  # check for name of latent in dataset
  if (!(latent %in% names(data))) stop("The latent variable you're asking for doesn't exist.")
  
  # return the plot
  latent_flexplot(data, latent, ...)
}

#p %>% modify_smooth(method="quadratic")
modify_smooth = function(p, method="lm", se=F,...) {
  # delete existing smoothing layers
  p = remove_geom(p, "GeomAbline")
  method_call = smooth_method_check(method="quadratic")
  p + suppressMessages(eval(parse(text=method_call)))
}

smooth_method_check = function(method) {
  if (method=="rlm") return('geom_smooth(method = "rlm", se = se, formula = y~x)')
  if (method=="poisson" | method=="Gamma") return('geom_smooth(method = "glm", method.args = list(family = "poisson"), se = se, formula = y~x)')
  if (method=="polynomial" | method == "quadratic") return('stat_smooth(method="lm", se=se, formula=y ~ poly(x, 2, raw=TRUE))')
  if (method=="cubic") return('stat_smooth(method="lm", se=se, formula=y ~ poly(x, 3, raw=TRUE))')
  if (method=="lm") return('stat_smooth(method="lm", se=se, formula = y~x)')
  return('geom_smooth(method="loess", se=se, formula = y~x)')
}

remove_geom <- function(ggplot2_object, geom_type) {
  # Delete layers that match the requested type.
  
  layers <- lapply(ggplot2_object$layers, function(x) {
    if (class(x$geom)[1] == geom_type) {
      NULL
    } else {
      x
    }
  })
  # Delete the unwanted layers.
  layers <- layers[!sapply(layers, is.null)]
  ggplot2_object$layers <- layers
  ggplot2_object
}