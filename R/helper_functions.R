get_legend = function(object2) {
  if (is.null(object2)) return(NULL) else return(c(1,2))
}

set_model_class = function(..., plot) {
  x = list(...)
  structure(x, class=plot)
}