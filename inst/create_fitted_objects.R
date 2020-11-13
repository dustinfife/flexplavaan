d = read.csv("data/health_depression.csv")
model = "
internet =~ Salience + ExcessiveUse + NeglectWork + Anticipation + LackofControl + NeglectSocialLife
health =~ Nutrition + Healthresponsibility + Selfactualization + Interpersonalsupport + Exercise + Stressmanagement
CESD~internet + health
internet ~~ health
"
health = sem(model, d)
implied_measurement(health, "internet",sort_slopes=F)[[1]] + coord_cartesian(ylim=c(-3, 3))
implied_measurement(health, "health")
  # some curvilinearity between NeglectSocialLife/Salience
implied_measurement(health, "internet")


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


