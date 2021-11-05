# this function takes an existing plot then changes the names of the plots
#plot_scatter_matrix(fit_bollen$lavaan, subset=1:3, plot="all") %>% modify_model_names(c("a", "b"))
#plot_scatter_matrix(fit_twofactor$lavaan, fit_twofactor_2$lavaan, subset=c("x1", "x2", "x3"), plot="all") %>%
# modify_model_names(c("a", "b"))
modify_model_names = function(p, model_names=c("a", "b")) {
  if(class(p)[1] != "gg") stop("This function only takes a plot as input.")
  return(p + scale_color_hue(labels = model_names))
}

# p = latent_plot(fit_bollen, formula = Eta2 ~ Eta1)
# formula = Eta1~Eta2
modify_formula = function(p, formula) {
  if(class(p)[1] != "gg")      stop("This function only takes a plot as input.")
  if(class(formula)[1] != "formula") stop("This function requires a valid formula.")
  
  # check whether variables in formula are in dataset
  # merge datasets from the plot
  if (length(p)>1) {
    
  }
  check_formula_in_data(p[[1]]$data, formula)
  data = p[[1]]$data
  head(p[[1]]$data )
}

merge_plot_data = function(p) {
  # find variables common among both
  same_variables = unique(c(names(p[[1]]$data), names(p[[1]]$data)))
  if (length(p)==1) return(p)
  join_all()
}
require(tidyverse)
p %>% map(pluck("data")) %>% map(f)

f = function(x, cols) {
  return(x[,cols])
}
%>% str#%>% lmap(pluck(1))
  str 



purrr::reduce(inner_join, by=same_variables) %>% head
head(p[[1]]$data)
?join
obj1 <- list("a", list(1, elt = "foo"))
obj2 <- list("b", list(2, elt = "bar"))
x <- list(obj1, obj2)
map(x, pluck, 2)
# extract names from formula
# look for variables (and variables with se) in both datasets
# merge those datasets (is that necessary?...probably, so I can )