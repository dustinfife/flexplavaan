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
  
  if(class(p)[2] == "ggmatrix") {
    str(p)
  }
  
  # get a list of all plots
  n_of_plots = length(p$plots)
  for (i in 1:n_of_plots){
    attr(p$plots[[i]]$fn, "params")$fit.lavaan = fit_bollen
    #attr(p$plots[[i]]$fn, "params")$method = "quadratic"
  }
  p[1,2]
  p + add_to_ggmatrix(p, geom_smooth(method="lm"), location="upper")
  p$plots[[i]]$fn$fit.lavaan
  attr(p$plots[[i]]$fn, "params")$method
  p$plots[[1]]
  attributes(p$plots[[1]]$fn)
  attr(p$plots[[1]]$fn, "params")$method = "quadratic"
 attr(p$plots[[1]], "fn")
  # delete existing smoothing layers
  p = remove_geom(p, "GeomAbline")
  method_call = smooth_method_check(method="quadratic")
  p + suppressMessages(eval(parse(text=method_call)))
}
#p + remove_and_add_gg_element(p, geom_smooth(method="lm"), location="upper")
remove_and_add_gg_element = function (e1, e2, location = NULL, rows = NULL, cols = NULL) 
{
  # if (!is.ggmatrix(e1)) 
  #   stop("e1 should be a ggmatrix.")
  # if (!is.ggproto(e2)) 
  #   stop("e2 should be a ggproto object.")
  pm <- e1
  gg <- e2
  loc <- GGally::ggmatrix_location(pm, location = location, rows = rows, 
                                             cols = cols)

    row_vals <- loc$row
  col_vals <- loc$col
  for (i in seq_along(row_vals)) {
    row <- row_vals[i]
    col <- col_vals[i]
    try({
      pm[row, col] <- remove_geom(pm[row, col], "GeomAbline")
      pm[row, col] <- remove_geom(pm[row, col], "GeomSmooth")
      pm[row, col] <- remove_geom(pm[row, col], "GeomLine")
      q = pm[row, col]
    })
  }
  pm
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