    #### this script simulates the non-linear data

set.seed(1212)
### latent variable of "force"'ness
latent = rnorm(1111)

### observed variables
x1 = 10 + .75*(2)*latent + rnorm(length(latent), 0, 2)
x2 = 30 + .55*(8)*latent + rnorm(length(latent), 0, 8)
x3a = 10^latent + rchisq(length(latent), 15, 20)
x3b = 50 + .65*(12)*latent + rnorm(length(latent), 0, 12)


### write to a dataframe
d = data.frame(x1=x1, x2=x2, x3a=x3a, x3b=x3b, latent=latent)
write.csv(d, "data/exp_data.csv", row.names=F)
