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

x2_2 = x2

extra.fit = "
for (i in 1:N){
  mu_2[i,3] <- nu[3,1,g[i]]^eta[i,1]
  x3a_2[i] ~ dnorm(mu_2[i,3], 1/theta[3,3,g[i]])
  mu_2[i,6] <- nu[6,1,g[i]]^eta[i,2]
  y3a_2[i] ~ dnorm(mu_2[i,6], 1/theta[6,6,g[i]])  
  mu2_eta[i,2] <- alpha[2,1,g[i]] + beta[2,1,g[i]]*eta[i,1]
}"
## fit the nonlinear dataset with nonlinear equation
fit.custom.nonlinear = bcfa(model.linear, data=d,
                            jagcontrol=list(method="rjparallel"),
                            mcmcextra = list(monitor="eta"),
                            target = "jags", test = "none")
saveRDS(fit.custom.nonlinear, file="data/custom_bayes_fit_linear.rds")



