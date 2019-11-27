require(blavaan); require(flexplot); require(tidyverse)
data("crossloadings_small")
data("crossloadings_large")

model_correct = "
f1 =~ x1 + x2 + x3 + y3
f2 =~ y1 + y2 + y3
f1 ~ f2
"
model_missspecified = "
f1 =~ x1 + x2 + x3
f2 =~ y1 + y2 + y3
f1 ~ f2
"

bayes_fit_crossloadings_correct = bcfa(model_correct, 
                               data=crossloadings_small,
                               save.lvs = TRUE, 
                               test = "none")
saveRDS(bayes_fit_crossloadings_correct, file="data/bayes_fit_crossloadings_correct.rds")

lvs = data.frame(blavInspect(bayes_fit_crossloadings_correct, 'lvmeans'))
names(lvs) = c("f1", "f2")
flexplot(f2~f1, data=lvs)
summary(bayes_fit_crossloadings_correct, standardized=TRUE)
medians = blavInspect(bayes_fit_crossloadings_correct, 'postmedian')
lvs$x1 = lvs$f1*1 + 9.965
lvs$x2 = lvs$f1*2.23 + 19.899
lvs$x3 = lvs$f1*3.361 + 30.119
head(lvs)
flexplot(x1~x2, data=crossloadings_small) +
  geom_line(data=lvs, aes(x2, x1))


str(mm)
mm = blavInspect(bayes_fit_crossloadings_correct, 'mcmc')
d = mm %>% spread_draws(ly_sign[i], Nu_free[i], regex=T) %>% 
  setNames(gsub("ly_sign", "lambda", names(.))) %>% 
   setNames(gsub("Nu_free", "nu", names(.))) %>% 
  group_by(i) %>% 
  summarize(lambda = mean(lambda), nu = mean(nu))

summary(bayes_fit_crossloadings_correct)
 
 
setNames(d, gsub("ly_sign", "lambda", names(d)))
str(mm)
require(tidybayes)

?spread_draws
f = function(i, mm) {
  d = mm[[i]] %>% unlist %>% as.data.frame %>% setNames()
  return(d)
}

1:3 %>% map(f, mm) %>% as.data.frame %>% head
mm[[1]]
#mm = 
  
  
  mm %>% flatten
  map(unlist) %>% as.data.frame  
str(mm[[1]])


bayes_fit_crossloadings = bcfa(model_missspecified, 
                               data=crossloadings_small,
                               save.lvs = TRUE, 
                               test = "none")
saveRDS(bayes_fit_crossloadings, file="data/bayes_fit_crossloadings.rds")

lvs = data.frame(blavInspect(bayes_fit_crossloadings, 'lvmeans'))
names(lvs) = c("f1", "f2")
flexplot(f2~f1, data=lvs)
summary(bayes_fit_crossloadings, standardized=TRUE)
medians = blavInspect(bayes_fit_crossloadings, 'postmedian')

medians
cov(lvs)

medians = blavInspect(bayes_fit_crossloadings, 'postmedian')

lvs %>% spread_draws() %>% head

bayes_fit_crossloadings@external$mcmcout$mcmc
bayes_fit_crossloadings@
  
  saveRDS(bayes_fit_crossloadings, file="data/bayes_fit_crossloadings.rds")
str()
blavInspect(bayes_fit_crossloadings, 'lvs')[[1]]
