d = read.csv("data/health_depression.csv")
model = "
internet =~ Salience + ExcessiveUse + NeglectWork + Anticipation + LackofControl + NeglectSocialLife
health_physical =~ Nutrition + Healthresponsibility +  Exercise 
health_emotional =~ Selfactualization + Interpersonalsupport + Stressmanagement
CESD~internet + health_physical + health_emotional
"
health_old = sem(model, d)

  
  # this model adds Stressmanagement to physical health and nutrition to emotional
model = "
internet =~ Salience + ExcessiveUse + NeglectWork + Anticipation + LackofControl + NeglectSocialLife+ Nutrition
health_physical =~ Nutrition + Exercise + Healthresponsibility+ Stressmanagement
health_emotional =~ Selfactualization + Interpersonalsupport + Stressmanagement + Nutrition
CESD~internet + health_physical + health_emotional
health_physical ~ health_emotional
"
health = flexplavaan(model, d)
usethis::use_data(health, overwrite=T)


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



