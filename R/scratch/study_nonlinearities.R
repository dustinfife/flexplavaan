# I'm going to see how nonlinear/nonnormality show up in indicator relationships

# 1. Non-normally distributed latent, but linear model
n = 5000
latent = rnorm(n, 3, .5) + 1*rexp(n) %>% scale
x1 = .5*latent + rnorm(length(latent), 0, sqrt(1-.5^2))
x2 = .5*latent + rnorm(length(latent), 0, sqrt(1-.5^2))
d = data.frame(x1, x2)
a = flexplot(x1~x2, data=d) + labs(subtitle = "Skewed Distribution")
a

  # so simple skewness leads to nonlinearity! (Somewhat, at least)


# 2. Normally distributed latent, but nonlinear link function
n = 5000
latent = rnorm(n, 0,1) 
x1 = .4*latent -.3*latent^2 + rnorm(length(latent), 0, sqrt(1-.5^2))
x2 = .4*latent -.3*latent^2 + rnorm(length(latent), 0, sqrt(1-.5^2))
d = data.frame(x1, x2, latent)
b = flexplot(x2~x1, data=d) + labs(subtitle = "Nonlinear relationship between latent/observed")
# so how would you tell whether it's nonnormal or nonlinear?


# 3. Normally distributed latent, but nonlinear link function
n = 5000
latent = rnorm(n, 0,1) 
x1 = .4*latent + rnorm(length(latent), 0, sqrt(1-.5^2))
x2 = .4*latent + .3*x1 -.1*x1^2 + rnorm(length(latent), 0, sqrt(1-.5^2))
d = data.frame(x1, x2, latent)
c = flexplot(x2~x1, data=d) + labs(subtitle = "Nonlinear relationship between observed")
c
require(patchwork)
a + b + c
  # I don't think it will be easy to tell the source of nonlinearities. And maybe it doesn't matter.
  # Maybe we just want to describe its nature. 



# explore nonnormality more
# 1. Non-normally distributed latent, but linear model
n = 5000
latent = rnorm(n, 3, 1) %>% scale
x1 = .5*latent + rnorm(length(latent), 0, sqrt(1-.5^2))
x2 = .5*latent + rnorm(length(latent), 0, sqrt(1-.5^2))
d = data.frame(x1, x2)
a1 = flexplot(x1~x2, data=d) + labs(subtitle = "normal")
n = 5000
latent = rnorm(n, 3, 1) + -1*rexp(n) %>% scale
x1 = .5*latent + rnorm(length(latent), 0, sqrt(1-.5^2))
x2 = .5*latent + rnorm(length(latent), 0, sqrt(1-.5^2))
d = data.frame(x1, x2)
a2 = flexplot(x1~x2, data=d) + labs(subtitle = "normal + skewed")
n = 5000
latent = rnorm(n, 3, .5) + -1*rexp(n) %>% scale
x1 = .5*latent + rnorm(length(latent), 0, sqrt(1-.5^2))
x2 = .5*latent + rnorm(length(latent), 0, sqrt(1-.5^2))
d = data.frame(x1, x2)
a3 = flexplot(x1~x2, data=d) + labs(subtitle = ".5 normal + skewed")
n = 5000
latent =  -1*rexp(n) %>% scale
x1 = .5*latent + rnorm(length(latent), 0, sqrt(1-.5^2))
x2 = .5*latent + rnorm(length(latent), 0, sqrt(1-.5^2))
d = data.frame(x1, x2)
a4 = flexplot(x1~x2, data=d) + labs(subtitle = "skewed")
a1 + a2 + a3 + a4

