fit.lavaan = readRDS(file="data/hogwarts_summary.rds")
data(hogwarts_survival)
data = hogwarts_survival
y = "potions"
x = "darkarts"

## estimate latent variables
lvs = startsWith(dimnames(fit.lavaan)[[1]], "eta")
lvs = data.frame(fit.lavaan[lvs,"Mean"] )
names(lvs) = "f"

newdata = cbind(data, lvs) %>% 
  data.frame %>% 
  mutate(!!"yhat" := fifer::rescale(f, mean(!!(as.name(y))), sd(!!(as.name(y)))))


var_explained = var(residualize.lowess(newdata$f, newdata[,x], return.fitted=T)-mean(newdata[,x]))
var_total = var(newdata[,x])
rel.x = (var_explained/var_total) ### square of correlation = reliability

var_explained = var(residualize.lowess(newdata$f, newdata[,y], return.fitted=T)-mean(newdata[,y]))
var_total = var(newdata[,y])
rel.y = (var_explained/var_total)

x_new = quantile(newdata[,x], probs = seq(from=0, to=1, length.out=80)) %>% as.numeric
y_new = quantile(newdata[,"f"], probs = seq(from=0, to=1, length.out=80)) %>% as.numeric
y_new = rescale(y_new, new.mean = mean(newdata[,y]), new.sd = sd(newdata[,y]))
y_new2 = mean(y_new) + sqrt(rel.x*rel.y)* (y_new-mean(y_new))
mean(y_new)
mean(y_new2)
# transform f to reduce it's slope by reliability
fnew = newdata[["f"]]
newdata[["f"]] = mean(fnew) + sqrt(rel.x*rel.y) * (fnew-mean(fnew))    
residuals = newdata[[y]] - newdata[["f"]]

form = flexplot::make.formula(y, x)
flexplot(form, data=data) +
  geom_line(data=data.frame(x_new=x_new, y_new=y_new), aes(x_new, y_new), col="red") +
  geom_line(data=data.frame(x_new=x_new, y_new=y_new2), aes(x_new, y_new), col="blue")









#
