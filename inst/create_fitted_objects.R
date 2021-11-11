# create a small model for tests
require(tidyverse)
set.seed(2323)
n = 50
f1 = rnorm(n)
f2 = .4*f1 + rnorm(n, 0, sqrt(1-.4^2))
z = .3*f1 + .4*f2 + rnorm(n, 0, .3)
f = function(x, f1) return(x = x*f1 + rnorm(length(f1), 0, sqrt(1-x^2)))
x = c(.6,.7,.8) %>% 
  purrr::map_dfc(f, f1) %>%
  set_names(paste0("x", 1:3))
y = c(.6,.7,.8) %>% 
  purrr::map_dfc(f, f2) %>%
  set_names(paste0("y", 1:3))
d = cbind(x, y, z)
head(d)

small_syntax = '
f1 =~ x1 + x2 + x3
f2 =~ y1 + y2 + y3
z ~ f1 + f2
'
small_mis_syntax = 
'
f1 =~ x1 + x2 
f2 =~ y1 + y2 + y3 + x3
z ~ f1 + f2
'

small_diflat_syntax = 
  '
f1 =~ x1 + x2 
F =~ y1 + y2 + y3 + x3
z ~ f1 + F
'

small_fa_syntax = '
f1 =~ x1 + x2 + x3
f2 =~ y1 + y2 + y3
'
  

small_data = d
small = lavaan::sem(small_syntax, d)
small_mis = lavaan::sem(small_mis_syntax, d)
small_diflat = lavaan::sem(small_diflat_syntax, d)
small_fa = lavaan::sem(small_fa_syntax, d)
small_flexplavaan = flexplavaan(small_syntax, d)
usethis::use_data(small, overwrite=T)
usethis::use_data(small_mis, overwrite=T)
usethis::use_data(small_syntax, overwrite=T)
usethis::use_data(small_mis_syntax, overwrite=T)
usethis::use_data(small_data, overwrite=T)
usethis::use_data(small_flexplavaan, overwrite=T)
usethis::use_data(small_diflat, overwrite=T)
usethis::use_data(small_fa, overwrite=T)

# simulate data according to Figure 2 in paper
set.seed(12121)
f1 = rnorm(1000)
a = .6; b = .7
c1 = .3; c2 = .4
x1 = a*f1 + rnorm(length(f1), 0, sqrt(1-a^2))
x2 = b*f1 + rnorm(length(f1), 0, sqrt(1-b^2))
res_err = sqrt(1-(c1^2 + c2^2 + 2*c1*c2*a*b))
x3 = c1*x1 + c2*x2 + rnorm(length(f1), 0, res_err)
d = data.frame(x1, x2, x3)

  # specify incorrect model
mod = "
f =~x1 + x2 + x3
"
  # specify correct model
mod2 = "
f =~x1 + x2
x3 ~~ x1
x2 ~~x3
"

# mod_bad = sem(mod, data=d)
# mod_good = sem(mod2, data=d)
# residuals(mod_bad, type="cor")$cov 
# residuals(mod_good, type="cor")$cov 
# summary(mod_bad)
# summary(mod_good)


#visualize(mod_bad, mod_good)
#implied_measurement(mod_good, mod_bad)
implied_measurement(mod_bad)
#summary(mod_bad)
#summary(mod_good)
#residuals(mod_good, type="cor")$cov 
just_identified = d
ji_model_correct = mod_good
ji_model_incorrect = mod_bad
usethis::use_data(just_identified, overwrite=T)
usethis::use_data(ji_model_correct, overwrite=T)
usethis::use_data(ji_model_incorrect, overwrite=T)



d = read.csv("data/health_depression.csv")
require(tidyverse)
flexplot(NeglectWork~1, data=d)
require(MASS)
boxcox(NeglectWork~1, data=d)
d = d %>% mutate(
  Salience = log(Salience),
  ExcessiveUse = log(ExcessiveUse), 
  NeglectWork = (NeglectWork)^(-.8),
  Anticipation = log(Anticipation), 
  LackofControl = log(LackofControl), 
  NeglectSocialLife = log(NeglectSocialLife)
)
flexplot(NeglectWork~1, data=d)


pl = names(d)
model = "
internet =~ Salience + ExcessiveUse + NeglectWork + Anticipation + LackofControl + NeglectSocialLife
health_physical =~ Nutrition + Healthresponsibility +  Exercise 
health_emotional =~ Selfactualization + Interpersonalsupport + Stressmanagement
CESD~internet + health_physical + health_emotional
"
health_old = flexplavaan(model, d)
pl = lavNames(health_old$lavaan)
summary(health_old$lavaan, fit.measures=T)
residual_plots(health_old, max_val = .05)
implied_measurement(health_old, latent="internet", limit = 6, method="lm")[[1]] + geom_smooth(col="black", method="lm", formula=y ~ poly(x, 2, raw=TRUE), se=F)
  # ok, but maybe add nutrition and internet?
implied_measurement(health_old, latent="health_physical", limit=6, method="lm")[[1]] + geom_smooth(col="black", method="lm", formula=y ~ poly(x, 2, raw=TRUE), se=F)
  # add stress to physical
implied_measurement(health_old, latent="health_emotional", limit = 6, method="lm")[[1]] + geom_smooth(col="black", method="lm", formula=y ~ poly(x, 2, raw=TRUE), se=F)
  # significant curves with salience and neglect social life
visualize(health_old)
visualize(health_old, plot="latent", bins=2)
  

d$LackofControlsq = scale(d$LackofControl)^2
d$Saliencesq = scale(d$Salience)^2
model = "
internet =~ Salience + ExcessiveUse + NeglectWork + Anticipation + LackofControl + NeglectSocialLife
internet_sq =~ Saliencesq + LackofControlsq
health_physical =~ Healthresponsibility +  Exercise + Stressmanagement
health_emotional =~ Selfactualization + Interpersonalsupport + Stressmanagement
Nutrition ~ health_physical + health_emotional + internet
CESD~internet + health_physical + health_emotional + internet_sq
"

# lack of control and salience
health = flexplavaan(model, d)
summary(health$lavaan, fit.measures=T, standardized=T)
visualize(health, subset=sample(pl, size=4), sort_plots = F, method="quadratic")
implied_measurement(health, latent="internet", limit = 9, method="lm")[[1]] + geom_smooth(col="black", method="lm", formula=y ~ poly(x, 2, raw=TRUE), se=F)
implied_measurement(health, latent="health_physical", limit=9, method="lm")[[1]] + geom_smooth(col="black", formula=y ~ poly(x, 2, raw=TRUE), method="lm", se=F)
implied_measurement(health, latent="health_emotional", method="lm")[[1]]+ geom_smooth(col="black", formula=y ~ poly(x, 2, raw=TRUE), method="lm", se=F)
visualize(health, plot="latent", formula = CESD~internet_sq)

integrate(dnorm, -1.96, 1.96)

model = "
internet =~ Salience + ExcessiveUse + NeglectWork + Anticipation + LackofControl + NeglectSocialLife
internet_sq =~ Saliencesq + NeglectSocialLifesq
health_physical =~ Nutrition + Healthresponsibility +  Exercise 
health_emotional =~ Selfactualization + Interpersonalsupport 
health_physical + health_emotional ~ Stressmanagement
health_emotional ~~ internet_sq
health_emotional ~~ internet
health_physical ~~ internet
health_emotional ~~ health_physical
CESD~internet + health_physical + health_emotional + Stressmanagement
"
health = flexplavaan(model, d)
summary(health$lavaan, fit.measures=T)
visualize(health, subset=1:4, sort_plots = F, method="quadratic")
implied_measurement(health, latent="internet", limit = 9, method="lm")[[1]] + geom_smooth(col="black", method="lm", formula=y ~ poly(x, 2, raw=TRUE), se=F)
implied_measurement(health, latent="health_physical", limit=9, method="lm")[[1]] + geom_smooth(col="black", formula=y ~ poly(x, 2, raw=TRUE), method="lm", se=F)
implied_measurement(health, latent="health_emotional", method="lm")[[1]]+ geom_smooth(col="black", formula=y ~ poly(x, 2, raw=TRUE), method="lm", se=F)
visualize(health, plot="latent")

usethis::use_data(health, overwrite=T)

d = read.csv("data/health_depression.csv")
d$Saliencesq = scale(d$Salience)^2
d$ExcessiveUsesq = scale(d$ExcessiveUse)^2
d$LackofControlsq = scale(d$LackofControl)^2
d$NeglectWorksq = scale(d$NeglectWork)^2
d$Anticipationsq = scale(d$Anticipation)^2
d$NeglectSocialLifesq = scale(d$NeglectSocialLife)^2

d$Nutritionsq = scale(d$Nutrition)^2
d$Exercisesq = scale(d$Exercise)^2
d$Healthresponsibilitysq = scale(d$Healthresponsibility)^2
d$CESDsq = scale(d$CESD)^2

d$Selfactualizationsq = scale(d$Selfactualization)^2
d$Interpersonalsupportsq = scale(d$Interpersonalsupport)^2
d$Stressmanagementsq = scale(d$Stressmanagement)^2


model = "
internet =~ Salience + ExcessiveUse + NeglectWork + Anticipation + LackofControl + NeglectSocialLife
internet_sq =~ Saliencesq + ExcessiveUsesq + LackofControlsq + NeglectWorksq + Anticipationsq 
health_physical =~ Nutrition + Exercise + Healthresponsibility
health_physical_sq =~ Exercisesq + Healthresponsibilitysq
health_emotional =~ Selfactualization + Interpersonalsupport + Stressmanagement 
health_emotional_sq =~ Selfactualizationsq + Interpersonalsupportsq + Stressmanagementsq 
CESD~internet + health_physical + health_emotional + internet_sq + health_physical_sq + health_emotional_sq
CESDsq~internet + health_physical + health_emotional + internet_sq + health_physical_sq+ health_emotional_sq
"

health_poly = flexplavaan(model, d)
visualize(health_poly, subset=c("NeglectWork", "NeglectSocialLife", "Anticipation", "Exercise", "Stressmanagement", "CESD"))
implied_measurement(health_poly, latent="internet", limit = 9, method="lm")[[1]] + geom_smooth(col="black", method="lm", formula=y ~ poly(x, 2, raw=TRUE), se=F)
implied_measurement(health_poly, latent="health_physical", limit=9, method="lm")[[1]] + geom_smooth(col="black", formula=y ~ poly(x, 2, raw=TRUE), method="lm", se=F)
## seems to be a nonlinear between internet and health_physical
implied_measurement(health_poly, latent="health_emotional", method="lm")[[1]]+ geom_smooth(col="black", formula=y ~ poly(x, 2, raw=TRUE), method="lm", se=F)
visualize(health_poly, plot="latent", formula = CESD~internet)
visualize(health_poly, plot="latent", formula = CESD~health_emotional)
visualize(health_poly, plot="latent", formula = CESD~health_physical)










usethis::use_data(health, overwrite=T)
lavNames(health_poly$lavaan, type="lv")
latents = lavPredict(health_poly$lavaan)
m = cbind(latents,d)
m$DEP = m$CESD + m$CESDsq
m$int = m$internet + m$internet_sq
flexplot(DEP~int, data=m)
coef(health_poly$lavaan)
data("correct_small")
model_1 = "
f1 =~ x1 + x2 + x3
f2 =~ y1 + y2 + y3
f1 ~ f2
"
model_2 = "
f1 =~ x1 + x2 + x3
f2 =~ x3 + y1 + y2 + y3
f1 ~ f2
"

fit_twofactor = flexplavaan(model_1, data=correct_small)
fit_twofactor_2 = flexplavaan(model_2, data=correct_small)
usethis::use_data(fit_twofactor, overwrite=T)
usethis::use_data(fit_twofactor_2, overwrite=T)



data("PoliticalDemocracy")
democ1 <- '
Eta1 =~ y1 + y2 + y3 + y4
Eta2 =~ y5 + y6 + y7 + y8
Xi1 =~ x1 + x2 + x3
Eta1 ~ Xi1
Eta2 ~ Xi1
Eta2 ~ Eta1
y1 ~~ y5
y2 ~~ y4
y2 ~~ y6
y3 ~~ y7
y4 ~~ y8
y6 ~~ y8
'

fit_bollen <- flexplavaan( democ1, data = PoliticalDemocracy)
usethis::use_data(fit_bollen, overwrite=T)



