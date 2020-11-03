d = read.csv("data/health_depression.csv")
model = "
internet =~ Salience + ExcessiveUse + NeglectWork + Anticipation + LackofControl + NeglectSocialLife
psych_health =~ Selfactualization + Interpersonalsupport + Stressmanagement 
health =~ Nutrition + Healthresponsibility + Exercise + Stressmanagement
CESD ~ psych_health + internet + health
internet ~~ health
internet ~~ psych_health
health ~~ psych_health
"

model2 = "
internet =~ Salience + ExcessiveUse + NeglectWork + Anticipation + LackofControl + NeglectSocialLife
psych_health =~ Selfactualization + Interpersonalsupport + Stressmanagement 
health =~ Nutrition + Healthresponsibility + Exercise
CESD ~ psych_health + internet + health
internet ~~ health
internet ~~ psych_health
health ~~ psych_health
"

# Fit the models ----------------------------------------------------------
require(lavaan)
require(ggplot2)
require(tidyverse)
stress_cross = sem(model, d)
stress_no_cross = sem(model2, d)

visualize(stress_cross, stress_no_cross,
          subset = 1:4,
          method="lm")


summary(health, fit.measures=T, standardized=T)



require(flexplavaan)
require(flexplot)
visualize(health ,    
          subset = c("Nutrition", "CESD", "NeglectWork", "ExcessiveUse"),
          suppress_smooth=F,
          method="lm")
flexplavaan:::residual_plots(health, max_val = .05) + coord_flip()
visualize(health, plot="measurement", alpha=.2)
visualize(health, plot="latent", alpha=.2)






ord = flexplavaan:::block_model_residuals(health)
cor_resid = lavResiduals(health, type="cor")$cov
cor_resid[ord, ord] %>% round(3)


eq = sna::equiv.clust(cor_resid, equiv.fun=sna::sedist)
bm = sna::blockmodel(cor_resid, eq, k = nrow(cor_resid), 
                mode="graph")
str(bm)
bm$plabels[bm$order.vector]
plot(bm)

visualize(health, plot="residuals") 

summary(health, standardized=T)
library(concoR)
concor_hca(cor_resid, p=3)
