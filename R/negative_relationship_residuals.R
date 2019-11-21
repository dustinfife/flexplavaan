fifer::clear()
n = 50000
slopes = c(.5, .5, .5)
latent = rnorm(n)

# create three indicators
x1 = slopes[1]*latent + rnorm(n,0, sqrt(1-slopes[1]^2))
x2 = slopes[2]*latent + rnorm(n,0, sqrt(1-slopes[2]^2))
x3 = slopes[3]*latent + rnorm(n,0, sqrt(1-slopes[3]^2))
d = data.frame(x1=x1, x2=x2, x3=x3)

# model in lavaan
model.linear = '
  A =~ x1 + x2 + x3
  A ~~ A
'
fitted = cfa(model.linear, data=d)
summary(fitted)
# create residuals
observed.vars = lavNames(fitted, type="ov")
lav_residuals = d[,observed.vars] - lavPredict(fitted, type="ov")

# plot(x3~x2, data=lav_residuals)
# abline(lm(x3~x2, data=lav_residuals))
# plot(x3~x1, data=lav_residuals)
# abline(lm(x3~x1, data=lav_residuals))
# plot(x1~x2, data=lav_residuals)
# abline(lm(x1~x2, data=lav_residuals))

# trace plots - the expected fit between x1 and x2, given the latent variable
  # new dataset that spans the range of x1
m = data.frame(x1 = seq(from=min(d$x1), to=max(d$x1), length.out=20))
  # expected value of x2 given x1
rel.x = slopes[1]^2; rel.y = slopes[2]^2
m$latent = (slopes[2]^2)*m$x1
plot(m$x1, m$latent)
ggplot(data=d, aes(x=x1,y=x2)) +
  geom_point() + 
  geom_line(data=m, aes(x1, latent), col="red") +
  geom_smooth(method="lm")

#### They're not the same because of MEASUREMENT ERROR
#### so I need to reduce the slope proportional to measurement error