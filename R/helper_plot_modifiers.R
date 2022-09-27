
# this function removes the smooth geom from a scatterplot matrix and adds the new one
#p + remove_and_add_gg_element(p, geom_smooth(method="lm"), location="upper")
remove_and_add_gg_element = function (plot, geom, location = NULL, rows = NULL, cols = NULL) 
{
  loc <- GGally::ggmatrix_location(plot, location = location, rows = rows, 
                                   cols = cols)
  
  row_vals <- loc$row
  col_vals <- loc$col
  
  for (i in seq_along(row_vals)) {
    row <- row_vals[i]
    col <- col_vals[i]
    if (row != col) {
      try({
        plot[row, col] <- remove_geom(plot[row, col], "GeomSmooth")
        plot[row, col] <- plot[row, col] + geom
      })
    }
  }
  plot
}

smooth_method_check = function(method=NULL) {
  if (is.null(method)) return ('geom_smooth(method="loess", se=se, formula = y~x)')
  if (method=="rlm") return('geom_smooth(method = "rlm", se = se, formula = y~x)')
  if (method=="poisson") return('geom_smooth(method = "glm", method.args = list(family = "poisson"), se = se, formula = y~x)')
  if (method=="Gamma") return('geom_smooth(method = "glm", method.args = list(family = "Gamma"), se = se, formula = y~x)')  
  if (method=="polynomial" | method == "quadratic") return('stat_smooth(method="lm", se=se, formula=y ~ poly(x, 2, raw=TRUE))')
  if (method=="cubic") return('stat_smooth(method="lm", se=se, formula=y ~ poly(x, 3, raw=TRUE))')
  if (method=="lm") return('stat_smooth(method="lm", se=se, formula = y~x)')
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