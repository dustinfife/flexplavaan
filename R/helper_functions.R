get_legend = function(object2) {
  if (is.null(object2)) return(NULL) else return(c(1,2))
}

set_model_class = function(object1_l, object2_l, names, legend, d, nms, plot) {
  x = list(object1_l=object1_l, 
           object2_l=object2_l, 
           names=names, 
           legend=legend, 
           nms=nms,
           d=d)
  structure(x, class=plot)
}