fifer::clear()
require(lavaan) 
require(ggplot2)
n = 500
slopes = c(.85, .85, .85)
latent = rnorm(n)

# create three indicators of magic
x1 = slopes[1]*latent + rnorm(n,0, sqrt(1-slopes[1]^2))
x2 = slopes[2]*latent + rnorm(n,0, sqrt(1-slopes[2]^2))
x3 = slopes[3]*latent + rnorm(n,0, sqrt(1-slopes[3]^2))
x3a = (30*(latent+5))/(0.01 + (latent+5)) #+ rnorm(length(latent), 0, 3)
plot(latent, x3a)
d = data.frame(x1=x1, x2=x2, x3=x3, midichlorian=x3a)

    ### x1 -> strange occurances score by independent observers
    ### x2 -> self-reported attention score
    ### x3 -> 

muggles = d