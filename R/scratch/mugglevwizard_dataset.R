fifer::clear()
require(lavaan) 
require(ggplot2)
n = 500

latent = rnorm(n)
latent2 = .8*latent + rnorm(n, 0, sqrt(1-.8^2))

# create three indicators of magic
x1 = 20 + .8*4*latent + rnorm(n,0, sqrt(1-.8^2)*4)
  mx = 6  ### maximum + of occurances
  mid = 1.5 ### midpoint (point at which half of max is reached)
  slope = 4 ### rate of increase per sd increase (at mid point)
x2 = round(mx/(1 + exp(-slope*(latent-mid)))  + rpois(n, 0))
  b0 = 1-6*min(latent) 
x3 = b0 + .9*6*latent + (rnorm(n, 0, sqrt(1-.9^2)*6))
x3[x3<0] = 0

y1 = 70 + .9*15*latent2 + rnorm(n,0, sqrt(1-.9^2)*15)
y2 = 70 + .6*15*latent2 + rnorm(n,0, sqrt(1-.6^2)*15)
y3 = 70 + .45*15*latent2 + rnorm(n,0, sqrt(1-.45^2)*15)

mugglevwizard = cut(latent, c(-Inf, 1.5, Inf), labels = c("muggle", "witch_wizard"))

gender = sample(c("boy", "girl"), size = 500, replace=T)

mugglevwizard = data.frame(strange=x1, relatives=x2, wingardium=x3, darkarts=y1, potions=y2, history=y3, mugglevwizard=mugglevwizard,
                           gender = gender)
require(dplyr)
mugglevwizard = mugglevwizard %>% mutate(mugglevwizard = ifelse(gender=="boy", "wizard", "witch"))  %>% select(-gender) %>% mutate_if(is.numeric, round)
usethis::use_data(mugglevwizard, overwrite = TRUE)





