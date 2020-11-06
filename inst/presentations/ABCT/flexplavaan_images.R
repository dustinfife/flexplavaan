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


p = visualize(force_fit, plot="model", 
              subset=c("exam_one", "exam_two", "exam_three"))

ggsave("inst/presentations/ABCT/screenshots/trace.jpg", plot=p)

p = visualize(force_fit,subset=c("exam_one", "exam_two", "exam_three"))

ggsave("inst/presentations/ABCT/screenshots/ddp.jpg", plot=p)
p = visualize(force_fit, plot="model", subset=c("exam_one", "exam_two", "exam_three"),
              suppress_smooth=F)

p = visualize(force_fit)

p = visualize(force_fit, plot="measurement",
              sample=100)
p$force_score


ggsave("inst/presentations/ABCT/screenshots/whole_mod.jpg", plot=p[[1]])


ggsave("inst/presentations/ABCT/screenshots/trace_loess.jpg", plot=p)
