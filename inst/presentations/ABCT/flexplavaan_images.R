require(tidyverse)
require(flexplot)
# t-test
t = lm(ptsd~north_south, data=avengers)
plot = visualize(t, plot="model")

ggsave("inst/presentations/ABCT/screenshots/t_test.jpg", plot=plot)


# ANOVA
anova = lm(weight.loss~therapy.type, data=exercise_data)
plot = visualize(anova, plot="model")

ggsave("inst/presentations/ABCT/screenshots/anova.jpg", plot=plot)

# regression
regression = lm(weight.loss~motivation, data=exercise_data)
plot = visualize(regression, plot="model")

ggsave("inst/presentations/ABCT/screenshots/regression.jpg", plot=plot)

# mixed model
require(lme4)
mixed = lmer(MathAch~SES+Minority + (SES | School), data=math)
plot = visualize(mixed, plot="model")

ggsave("inst/presentations/ABCT/screenshots/mixed.jpg", plot=plot)



data(jedi_jedi)

# specify the models ------------------------------------------------------
model = "
force_score =~ fitness + saber + midichlorian + force_history
jedi_score =~ exam_one + exam_two + exam_three
jedi_score ~ force_score
"

# Fit the models ----------------------------------------------------------
force_fit = cfa(model, jedi_jedi)
summary(force_fit, fit.measures=TRUE, standardized=TRUE)


p = visualize(force_fit, 
              plot="model",
              subset=c("fitness", "saber", "midichlorian", "force_history"),
              method="lm")
ggsave("inst/presentations/ABCT/screenshots/trace.jpg", plot=p)

p = visualize(force_fit,
              subset=c("fitness", "saber", "midichlorian", "force_history"),
              method="lm")

ggsave("inst/presentations/ABCT/screenshots/ddp_viz.jpg", plot=p)

p = visualize(force_fit)
ggsave("inst/presentations/ABCT/screenshots/whole_mod.jpg", plot=p)

p = visualize(force_fit, plot="measurement",
              sample=300)
p = p$force_score + theme_bw(base_size=18)
ggsave("inst/presentations/ABCT/screenshots/measurement.jpg", plot=p, 
       width = 7.49, height = 5)

p = visualize(force_fit, plot="latent")
p = p[[1]] + theme_bw(base_size=18)
ggsave("inst/presentations/ABCT/screenshots/latent.jpg", plot=p)




model2 = "
force_score =~ fitness + saber + midichlorian + force_history
jedi_score =~ exam_one + exam_two + exam_three + force_history
jedi_score ~ force_score
"

# Fit the models ----------------------------------------------------------
force_cross = cfa(model2, jedi_jedi)




summary(force_fit, fit.measures=TRUE, standardized=TRUE)


p = visualize(force_fit, force_cross,
              subset=c("fitness", "saber", "midichlorian", "force_history"),
              method="lm")
ggsave("inst/presentations/ABCT/screenshots/viz_model_2.jpg", plot=p)





install.packages("devtools")
devtools::install_github('dustinfife/flexplavaan')











