    #### this script simulates latent variables with nonlinear 
    #### relationships between each other and the indicators

set.seed(12121)
### latent variable of "force"'ness
latent_a = rnorm(3000)
    b0 = .5; b1 = .4;b2 = 0;# b2 =-1*b0
    e = sqrt(1-(b1^2+((b2^2)*2)))
latent_b = b0 + b1*latent_a + b2*(latent_a^2) + rnorm(length(latent_a), 0, e)

### observed variables
x1 = .75*latent_a + rnorm(length(latent_a), 0, sqrt(1-.75^2))
x2 = .55*latent_a + rnorm(length(latent_a), 0, sqrt(1-.55^2))
x3a = 3^latent_a + rnorm(length(latent_a), 0, 3)  ### nonlinear
x3b = .65*latent_a + rnorm(length(latent_a), 0, sqrt(1-.65^2))

y1 = .75*latent_b + rnorm(length(latent_b), 0, sqrt(1-.75^2))
y2 = .55*latent_b + rnorm(length(latent_b), 0, sqrt(1-.55^2))
y3a = 3^latent_b + rnorm(length(latent_b), 0, 3)  ### nonlinear
y3b = .65*latent_b + rnorm(length(latent_b), 0, sqrt(1-.65^2))

## this code allows me to model a different variable in jags to make it easier
#d$x3a_2 = d$x3a
#d$y3a_2 = d$y3a

### write to a dataframe
d = data.frame(x1=x1, x2=x2, x3a=x3a, x3b=x3b, 
               y1=y1, y2=y2, y3a=y3a, y3b=y3b, 
               latent_a=latent_a, latent_b=latent_b)
write.csv(d, "data/exp_data.csv", row.names=F)
head(d)
