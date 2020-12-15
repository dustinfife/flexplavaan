d = read.csv("data/health_depression.csv")
pl = names(d)
model = "
internet =~ Salience + ExcessiveUse + NeglectWork + Anticipation + LackofControl + NeglectSocialLife
health_physical =~ Nutrition + Healthresponsibility +  Exercise 
health_emotional =~ Selfactualization + Interpersonalsupport + Stressmanagement
CESD~internet + health_physical + health_emotional
"
health_old = flexplavaan(model, d)
pl = lavNames(health_old$lavaan)
residual_plots(health_old, max_val = .05)
implied_measurement(health_old, latent="internet", limit = 6, method="lm")[[1]] + geom_smooth(col="black", method="lm", formula=y ~ poly(x, 2, raw=TRUE), se=F)
  # ok
implied_measurement(health_old, latent="health_physical", limit=6, method="lm")[[1]] + geom_smooth(col="black", method="lm", formula=y ~ poly(x, 2, raw=TRUE), se=F)
  # add stress to physical
implied_measurement(health_old, latent="health_emotional", limit = 6, method="lm")[[1]] + geom_smooth(col="black", method="lm", formula=y ~ poly(x, 2, raw=TRUE), se=F)
  # significant curves with salience and neglect social life
visualize(health_old)
visualize(health_old, plot="latent", bins=2)
  

d$Saliencesq = scale(d$Salience)^2
d$NeglectSocialLifesq = scale(d$NeglectSocialLife)^2

model = "
internet =~ Salience + ExcessiveUse + NeglectWork + Anticipation + LackofControl + NeglectSocialLife
internet_sq =~ Saliencesq + NeglectSocialLifesq
health_physical =~ Nutrition + Healthresponsibility +  Exercise + Stressmanagement
health_emotional =~ Selfactualization + Interpersonalsupport + Stressmanagement
health_emotional ~~ internet_sq
health_emotional ~~ internet
health_physical ~~ internet
health_emotional ~~ health_physical
CESD~internet + health_physical + health_emotional
"
health = flexplavaan(model, d)
summary(health$lavaan)
visualize(health, subset=sample(pl, 6), sort_plots = F, method="quadratic")
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



