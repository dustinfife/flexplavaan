  #### prep the data
require(tidyverse)
require(flexplot)
data("hogwarts_survival")
hogwarts_nonlinear = readRDS(file="data/hogwarts_nonlinear.rds")
latents = export_jags_latents(hogwarts_nonlinear)
data = cbind(hogwarts_survival, latents)

visualize_nonlinear(data$darkarts, data$flying, data$factor2, plot="model")
visualize_nonlinear(data$darkarts, data$flying, data$factor2, plot="residual")

visualize_nonlinear(data$flying, data$darkarts, data$factor2, plot="model")
visualize_nonlinear(data$flying, data$darkarts, data$factor2, plot="residual")

visualize_nonlinear(data$herbology, data$potions, data$factor1, plot="model")
visualize_nonlinear(data$herbology, data$potions, data$factor1, plot="residual")



head(hogwarts_survival)
newpred = nonlinear_prediction(data$herbology, data$history, data$factor1) %>% data.frame
flexplot(history~herbology, data=data) +
  geom_line(data=newpred, aes(x, y), col="red")+
  geom_hline(yintercept = mean(data$herbology)) + 
  geom_vline(xintercept = mean(data$history)) 

newpred = nonlinear_prediction(data$spells, data$darkarts, data$factor2) %>% data.frame
flexplot(darkarts~spells, data=data) +
  geom_line(data=newpred, aes(x, y), col="red", size=2) + 
  geom_hline(yintercept = mean(data$spells)) + 
  geom_vline(xintercept = mean(data$darkarts)) 














#






























### this function computes the average of the latent variable within a specified
### range of x/y values. These values are NOT crossed so as to approximate the
### function of the latent variable
nonlinear_prediction = function(x,y,latent, points=10, rescale=T){
  xseq = sequence(x, length = points)
  yseq = sequence(y, length = points)
  xbin = cut(x, xseq)
  ybin = cut(y, yseq)
  latent_pred = rep(NA, times=length(xseq))
  for (i in 1:length(xseq)){
    rows = which(xbin==levels(xbin)[i] & ybin==levels(ybin)[i])
    if (sum(rows)>0){
      val = (median(latent[rows]))
    } else {
      val = NA
    } 
    latent_pred[i] = val
  }
  rxx = empirical_reliability(latent, x, loess=TRUE)
  ryy = empirical_reliability(latent, y, loess=TRUE)
  print(rxx)
  print(ryy)
  if (rescale){
    latent_pred = mean(y) + (latent_pred - mean(latent))*(sd(y)/sd(latent))*sqrt(rxx*ryy)
  }
  
  list(xbin=xseq, ybin=yseq, latent_pred=latent_pred)
}

newpred = data.frame(nonlinear_prediction(data$darkarts, data$spells, data$factor2))
flexplot(spells~darkarts, data=data) +
  geom_line(data=newpred, aes(xbin, latent_pred))

newpred = data.frame(
  nonlinear_prediction(data$spells, data$darkarts, data$factor2,points=10, rescale=F))
newpred$latent_pred = newpred$latent_pred + 25
flexplot(darkarts~spells, data=data) +
  geom_line(data=newpred, aes(xbin, latent_pred), col="red") +
  geom_vline(xintercept = c(28, 37, 46)) + 
  geom_hline(yintercept = c(11, 22, 33))


newpred = data.frame(nonlinear_prediction(data$potions, data$history, data$factor1))

flexplot(history~potions, data=data) +
  geom_line(data=newpred, aes(xbin, latent_pred))

head(data)
newpred = data.frame(nonlinear_prediction(data$herbology, data$potions, data$factor1))
flexplot(potions~herbology, data=data) +
  geom_line(data=newpred, aes(xbin, latent_pred))





















head(summary(hogwarts_nonlinear)[,"Mean", drop=FALSE], n=28)
  # lambda1 = slope of potions = 1
  # lambda2 = slope of history = 1.127
  # lambda3 = slope of herbology = .776
  # lambda4 = slope of spells = 1
  # lambda5 = slope of darkarts = .35 
  # lambda6 = slope of flying = 1.733
  # beta21 = slope between latents = .229
  # beta31 = slope for knowledge to survived = 1.99
  # beta32 = slope for skills to survived = 4.438
  # thetas = residual variances
  # psi[1,1] = precision of knowledge (1/102 = .009)
  # psi[2,2] = precision of knowledge (1/46.98 = .02)
  # nu1 = intercept (mean) of potions =  69
  # nu2 = intercept (mean) of history =  69
  # nu3 = intercept (mean) of herbology =  69
  # nu4 = intercept (mean) of spells =  70
  # nu5 = intercept (mean) of darkarts =  -10
  # nu6 = intercept (mean) of flying =  70
  # mx = 79.65

plot(latents$factor2, hogwarts_survival$darkarts)
x = sequence(x = latents$factor2)
y = 79.65/(1 + exp(-1*.35*(x+10)))
lines(x,y, col="red")

#### see if predicted value for d makes sense
# m = 79.65
# d1 = .35
# f = mean(latents$factor2)
# d0 = -10
# dpred = m/(1 + exp(-1*d1*(f - d0)))
# f - ((-1/d1)*log((m/dpred) -1) + d0)

compute_fitted = function(x,y,latent_x, latent_y, y0,x0, y1, x1, mx= NULL, my=NULL, fun = linear, plot=TRUE, ...){
  ### compute reliabilities
  #browser()
  rxx = empirical_reliability(latent=latent_x, observed=x, b0 = x0, b1=x1, mx=mx)
  ryy = empirical_reliability(latent=latent_y, observed=y, b0 = y0, b1=y1, mx=my)
  print(rxx)
  print(ryy)
  ### generate sequence
  #browser()
  x_new = sequence(x) + .1
  y_new = fun(x_new, mx=my, x0=x0, x1=x1, y0=y0, y1=y1, rxx=rxx, ryy=ryy)
  nd = data.frame(x=x_new, y=y_new)
  
  ### plot it
  if (plot){
    d = data.frame(x=x, y=y)
    flexplot(y~x, data=d, ...) + 
      geom_line(data=nd, aes(x,y))
  } else {
    return(nd)
  }
}
data = cbind(hogwarts_survival[,1:6], latents[,2:3])


x = hogwarts_survival$spells
xscaled = scale(x)
y = hogwarts_survival$darkarts
yscaled = scale(y)
xn = -3:3
yn = 1/(1 + exp(-.35*(xn)))
plot(xscaled, yscaled)
lines(xn, yn)

sd(latents$factor2*1 + 70)
sd(hogwarts_survival$spells)


d = data.frame(x=x,y=y)
a = compute_fitted(y = hogwarts_survival$darkarts, x = hogwarts_survival$spells,
               latent_y = latents$factor2, latent_x = latents$factor2,
               y0 = -9.9098, y1 = (.35), x0 = 70, x1 = 1, my = 79.65, mx=NULL,
               fun = logistic, plot=TRUE)  
 a+ geom_line(data=d, aes(x,y))


compute_fitted(x = hogwarts_survival$darkarts, y = hogwarts_survival$spells,
               latent_x = latents$factor2, latent_y = latents$factor2,
               x0 = 70, x1 = 1.733, y0 = 70, y1 = 1,
               fun = linear, plot=TRUE, method="lm") 



compute_fitted(x = hogwarts_survival$darkarts, y = hogwarts_survival$spells,
               latent_x = latents$factor2, latent_y = latents$factor2,
               x0 = -9.9098, x1 = .35, y0 = 70, y1 = 1, mx = 79.65,
               fun = inv.logistic, plot=TRUE) +
    geom_hline(yintercept = mean(hogwarts_survival$spells))+
    geom_vline(xintercept = mean(hogwarts_survival$darkarts))

head(hogwarts_survival)

x = hogwarts_survival$flying; y = hogwarts_survival$spells;
latent_x = hogwarts_survival$magic_skills; latent_y = hogwarts_survival$magic_skills;
x0 = 70; x1 = 1.733; y0 = 70; y1 = 1


x = hogwarts_survival$darkarts
y = hogwarts_survival$spells
latent_x = latents$factor2
latent_y = latents$factor2
x0 = -10
y0 = 70
x1 = .35
y1 = 1
mx = 79.65
fun.x = linear
fun.y = inv.logistic



predict_y = function(s0,s1,d0,d1,)
s0 = 70
s1 = 1
dseq = seq(.1, m-.1, length.out=20)
spred = s0 + s1*((-1/d1)*log((m/dseq) - 1) + d0)#; spred[is.infinite(spred)] = 77


newd = data.frame(x=dseq, y=spred)
flexplot(spells~darkarts, data=hogwarts_survival) +
  geom_line(data=newd, aes(dseq, spred))

### the other way around
sseq = create_sequence("spells", hogwarts_survival, T)
dpred = m/(1 + exp(-1*d1*((sseq-s0)/s1 - d0)))
newd = data.frame(x=sseq, y=dpred)
flexplot(darkarts~spells, data=hogwarts_survival) +
  geom_line(data=newd, aes(sseq, dpred))




### rsq between predicted factor
yhat_logistic = function(x, mx, b0, b1){
  mx/(1 + exp(-1*b1*(x - b0)))
}

compute_rsq = function(latent, observed, b0, b1, mx=NULL){
  if (!is.null(mx)) {
    predicted = mx/(1 + exp(-1*b1*(latent - b0)))
  } else {
    predicted = b0 + b1*latent
  }
  
  ss_reg = var(predicted - mean(observed))
  ss_tot = var(observed)
  ss_reg/ss_tot
}

dark_predicted = yhat_logistic(x=latents$factor2, mx=79.65, b0=-10, b1=.35)
ss_reg = var(dark_predicted - mean(hogwarts_survival$darkarts))
ss_tot = var(hogwarts_survival$darkarts)
rsq = ss_reg/ss_tot



d = cbind(hogwarts_survival, latents) 
d_gather = d %>% 
  mutate(factor2 = fifer::rescale(factor2, mean(magic_skills), sd(magic_skills)),
        factor1 = fifer::rescale(factor2, mean(magic_knowledge), sd(magic_knowledge))) %>% 
        gather(method_skills, value_skills, magic_skills, factor2) %>% 
        gather(method_knowledge, value_knowledge, magic_knowledge, factor1) %>% 
        mutate(method_skills = factor(method_skills, unique(method_skills), labels=c("Estimated", "Actual")),
               method_knowledge = factor(method_knowledge, unique(method_knowledge), labels=c("Estimated", "Actual"))) 

  ### plots of how they should look (with real factors)
flexplot(magic_skills~factor2, data=d)
flexplot(magic_knowledge~factor1, data=d)

flexplot(survived~value_skills + method_skills, d_gather, method="logistic")
flexplot(survived~value_knowledge + method_knowledge, d_gather, method="logistic")

flexplot(potions~value_knowledge + method_knowledge, d_gather)
flexplot(darkarts~value_skills + method_skills, d_gather)


slope_ab = function(x, b01, b02, b11, b12, max, rhoxx, rhoyy){
  b01 + b02 - (b11/b12)*sqrt(rhoxx*rhoyy)*(log(max+1) - log(x))
}

x = create_sequence("spells", d, sequence = TRUE)
slope_ab(x, 69, exp(-10), 1, .35, 79.65, cor(d$spells, d$magic_skills)^2, .8)


class(hogwarts_nonlinear)
fit.lavaan = summary(hogwarts_nonlinear)


data(hogwarts_survival)
data = hogwarts_survival
y = "flying"
x = "darkarts"

data$magic_skills = magic_skills
data$magic_knowledge = magic_knowledge

## estimate latent variables
lvs = startsWith(dimnames(fit.lavaan)[[1]], "eta")
lvs = data.frame(fit.lavaan[lvs,"Mean"] ) %>% setNames("factor_score") 
lvs$factor = dimnames(lvs)[[1]] %>% subsetString(",", 2) %>% gsub("]", "", .)
lvs$id = dimnames(lvs)[[1]] %>% subsetString(",", 1) %>% gsub("eta[", "", ., fixed=T) #%>% mutate(id=as.numeric(id))
lvs = lvs %>% spread(factor, factor_score) %>% setNames(c("id", paste0("factor", 1:(ncol(.)-1)))) %>% arrange(as.numeric(id))

newdata = cbind(data, lvs) %>% 
  data.frame 
plot(factor1~magic_knowledge, data=newdata)
plot(factor2~magic_skills, data=newdata)

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




s0 = 70
s1 = 1
d0 = -10
d1 = .35
m = 79.65
s0 - (s1/d1)*(log((m/mean(d$darkarts))-1) + d0)





dseq=88

dseq = create_sequence("darkarts", d, sequence=T)+.1
s0 + s1*((-1/d1)*log((m/dseq)-1) + d0)



#
