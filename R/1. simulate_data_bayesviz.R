    #### this script simulates the non-linear data

set.seed(1212)
### latent variable of "force"'ness
latent = rnorm(111)

### observed variables
x1 = 10 + .5*(2)*latent + rnorm(length(latent), 0, 2)
x2 = 30 + .25*(8)*latent + rnorm(length(latent), 0, 8)
x3 = 10^latent + rchisq(length(latent), 15, 20)
# plot(latent, x3)
# plot(x1, x3)
# rchisq()
### write to a dataframe
d = data.frame(x1=x1, x2=x2, x3=x3)
write.csv(d, "../data/exp_data.csv", row.names=F)
