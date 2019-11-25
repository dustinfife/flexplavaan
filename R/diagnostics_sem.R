#' Visualize Diagnostics for a lavaan object
#'
#' This function visualizes pairwise relationships between items in a lavaan
#' object. The user can specify whether to display "trace" or "disturbance" plots 
#' Trace plots will show the scatterplot between pairs of items (defined via the mapping argument)
#' with the model-implied fit overlaying the raw data. If the line fails to pass through the more
#' concentrated parts of the data, this will indicate misfit. The disturbance plots (or
#' disturbance-dependence plots) will show the relationship between the two items after 
#' removing the model-implied effects. If any relationship remains, it suggests the model failed
#' to capture important information. 
#' @param data The dataset containing the raw (item) observations
#' @param mapping A ggplot-defined mapping (e.g., \code{aes(x=x, y=y)}).
#' @param fit.lavaan A fitted \code{\link{lavaan::lavaan}} object
#' @param alpha The transparency of the datapoints in the scatterplot. Defaults to 0.5
#' @param plot One of the following: "trace" or "disturbance" 
#' @param ... Other arguments passed to \code{\link{ggplot2::ggplot}}
#'
#' @return a plot of the desired relationship
#' @export
#' @import flexplot ggplot2
#'
#' @examples
#' 

data("mugglevwizard")
### fit measurement model
model_witch = "
witch =~ a*strange + b*relatives + c*wingardium
witch ~~ witch
"
require(lavaan)
mod = cfa(model_witch, data=mugglevwizard)
viz_diagnostics(data = mugglevwizard, mapping = aes(wingardium, strange), fit.lavaan = mod, plot="trace")

#data = mugglevwizard; mapping = aes(relatives, strange); fit.lavaan = mod; plot="trace"
viz_diagnostics <- function(data, mapping, fit.lavaan, alpha=.5, plot=c("trace", "disturbance"), ...) {
  
  ### plot ggplot object so I can extract the elements
  p = ggplot(data=data, mapping=mapping)
  
  ### extract name of variable in aes
  y = p$labels$y
  x = p$labels$x
  
  ### extract name of latent variables
  observed = lavNames(fit.lavaan)
  latent.names = lavNames(fit.lavaan, type="lv")
  
  ### make a sequence for the x axis
  new.x = data.frame(x = seq(from=min(data[,x]), to=max(data[,x]), length.out=10))
  names(new.x) = x
  
  ### find index for the variable of interest
  y.location = which(observed==y)
  
  ## extract fit
  new_data = create_new_data(data, condition.vars=c(x,y))
  prediction = lavPredict(fit.lavaan, type="ov", newdata=new_data) %>% data.frame %>% head
  
  
  loadings = inspect(fit.lavaan,what="std")$lambda ## factor loading matrix
  b = loadings[y,]
  coef(fit.lavaan)
  ### estimate the fit (for the observed and new data)
  observed.pred = b^2*data[,x]
  new.x[,y] = b^2*new.x
  
  
  residual_name = paste0(y, "_resid")
  data[,residual_name] = data[,y] - observed.pred
  
  ### now add to ggplot object
  if (plot=="trace"){
    flexplot_form = flexplot::make.formula(y, x)
    flexplot::flexplot(flexplot_form, data=data, alpha=alpha, method="loess") + geom_line(data=new.x, aes_string(x,y), col="red")
  } else {
    flexplot_form = flexplot::make.formula(residual_name, x)
    flexplot::flexplot(flexplot_form, data=data, alpha=alpha, method="loess") + labs(y=paste0(y, " | latent")) + geom_hline(yintercept=0, col="red")
  }
}



#' Wizard/Witch Propensity Scores for 500 Students at Hogwarts 
#'
#' This dataset contains scores on various measures of wizarding ability
#' and their subsequent OWL scores at Hogwarts
#'
#' @format A data frame with 500 rows and 7 variables:
#' \describe{
#'   \item{strange}{Strange occurance scores on a 40 item survey, completed by an independent observer}
#'   \item{telekenesis}{Number of occurances reported by potential witch/wizard, self-reported}
#'   \item{wingardium}{Number of seconds the student was able to levitate an object using the spell "wingardium leviosa"}
#'   \item{darkarts}{Score at the final year of Hogwarts in "Defense Against the Dark Arts"}
#'   \item{potions}{Score at the final year of Hogwarts in "Potions"}
#'   \item{history}{Score at the final year of Hogwarts in "History of Magic"}
#'   \item{mugglevwizard}{Indicator variable that identifies each student as muggle or witch/wizard}
#'   
#' }
#' @source These data are proprietary and obtained through the school of Hogwarts. 
"mugglevwizard"


# 
# require(GGally); require(lavaan); require(flexplot)
# observed = lavNames(fit.lavaan)
# ggpairs(d[,observed], 
#         lower = list(continuous = wrap(my_bin,fit.lavaan = fit.lavaan, alpha = .2)),
#         upper = list(continuous = wrap(my_bin,fit.lavaan = fit.lavaan, alpha = .2, residuals=TRUE)),
#         )
# 
# 
# head(data)
# data[,c("latent_a_estimated", "latent_b_estimated")] = lavPredict(fit.lavaan, type="lv", method="Bartlett")
# data$resids = residuals(lm(x2~A, data=data))
# flexplot(resids~x1, data=data, method="lm")
# plo
# fit.lavaan$call
# head(data)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# flexplot(latent_b_estimated~latent_b, data=data)
# ### plot the relationship between x1 and x2
# 
# 
# ## residualize X2 based on latent variable
# d$x2_residual = residuals(lm(x2~latent_a, data=d))
# flexplot(x2_residual~x1, data=d)
# 
# 
# 
# 
# 
# 
# 
# #