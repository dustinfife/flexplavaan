d = read.csv("data/health_depression.csv")
model = "
internet =~ Salience + ExcessiveUse + NeglectWork + Anticipation + LackofControl + NeglectSocialLife
health_physical =~ Nutrition + Healthresponsibility +  Exercise 
health_emotional =~ Selfactualization + Interpersonalsupport + Stressmanagement
CESD~internet + health_physical + health_emotional
"
health_old = sem(model, d)
implied_measurement(health_old, "internet")[[1]] #+ coord_cartesian(ylim=c(-3, 3))
  # nutrition's off
implied_measurement(health_old, "health_physical")
  # this whole one is not as correlated as it should be with stress, exercise, neglectwork
implied_measurement(health_old, "health_emotional")
  # nutrition has stronger relationship with emotional health 

  
  # this model adds Stressmanagement to physical health and nutrition to emotional
model = "
internet =~ Salience + ExcessiveUse + NeglectWork + Anticipation + LackofControl + NeglectSocialLife+ Nutrition
health_physical =~ Nutrition + Exercise + Healthresponsibility+ Stressmanagement
health_emotional =~ Selfactualization + Interpersonalsupport + Stressmanagement + Nutrition
CESD~internet + health_physical + health_emotional
health_physical = health_emotional + health_emotional^2
"
health = sem(model, d)
summary(health, standardized=T, fit.measures=T)
implied_measurement(health, "internet")[[1]] #+ coord_cartesian(ylim=c(-3, 3))
# nutrition's off
implied_measurement(health, "health_physical")
# this whole one is not as correlated as it should be with stress, exercise, neglectwork
implied_measurement(health, "health_emotional")
# nutrition has stronger relationship with emotional health 
residual_plots(health, max_val = .05) + coord_flip()
visualize(health, health_old, subset=1:4)
visualize(health, plot="latent", formula = CESD~internet)
visualize(health, plot="latent", formula = CESD~health_physical)
visualize(health, plot="latent", formula = CESD~health_emotional)
visualize(health, plot="latent", formula = health_physical~health_emotional)

usethis::use_data(health)



require(lavaan)
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

fit_twofactor = cfa(model_1, data=correct_small)
fit_twofactor_2 = cfa(model_2, data=correct_small)
usethis::use_data(fit_twofactor)
usethis::use_data(fit_twofactor_2)



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

fit_bollen <- sem( democ1, data = PoliticalDemocracy)
usethis::use_data(fit_bollen)

model = "
force_score =~"


