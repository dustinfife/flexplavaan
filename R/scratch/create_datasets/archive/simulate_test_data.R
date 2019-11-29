population.model <- ' 
f1 =~ .7*x1 + 0.8*x2 + .9*x3 + .5*y3
f2 =~ .8*y1 + .8*y2 + .5*y3 
f1 ~ .4*f2
'

# generate data
set.seed(1234)
d <- simulateData(population.model, sample.nobs=10000, meanstructure = TRUE)

### change scaling
means = c(10, 20, 30, 1, 2, 3)
sds = c(2, 4, 6, 1, 1.5, 2)
d = 1:6 %>% purrr::map(~fifer::rescale(d[,.x], new.mean = means[.x], new.sd = sds[.x])) %>% 
  data.frame %>% 
  setNames(names(d))

crossloadings_large = d
crossloadings_small = d[1:1000,]
usethis::use_data(crossloadings_large, overwrite = TRUE)
usethis::use_data(crossloadings_small, overwrite = TRUE)


population.model <- ' 
f1 =~ .7*x1 + 0.8*x2 + .9*x3
f2 =~ .7*y1 + .8*y2 + .5*y3
f1 ~ .4*f2
'

# generate data
set.seed(1234)
d <- simulateData(population.model, sample.nobs=10000, meanstructure = TRUE)

### change scaling
means = c(10, 20, 30, 1, 2, 3)
sds = c(2, 4, 6, 1, 1.5, 2)
d = 1:6 %>% purrr::map(~fifer::rescale(d[,.x], new.mean = means[.x], new.sd = sds[.x])) %>% 
  data.frame %>% 
  setNames(names(d))

correct_large = d
correct_small = d[1:500,]
usethis::use_data(correct_large, overwrite = TRUE)
usethis::use_data(correct_small, overwrite = TRUE)
