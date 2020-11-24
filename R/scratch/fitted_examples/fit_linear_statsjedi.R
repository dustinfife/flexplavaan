require(lavaan)
require(flexplot)
require(flexplavaan)
require(kutils)
data(stats_jedi)

  ### specify models (fixed to one versus not)
model1 = "stats_jedi =~ exam_one + exam_two + exam_three"
model2 = "stats_jedi =~ NA*exam_one + exam_two + exam_three
stats_jedi ~~ 1*stats_jedi"

  ### fit models
stats_jedi_fit = cfa(model1, stats_jedi)
usethis::use_data(stats_jedi_fit, overwrite=T)
  summary(stats_fit1, fit.measures=TRUE, standardized=TRUE)
stats_fit2 = cfa(model2, stats_jedi)
  summary(stats_fit2, fit.measures=TRUE, standardized=TRUE)

  ### visualize the two (fit is identical)
visualize(stats_fit1, stats_fit2, suppress_smooth=F)








# Graphics for presentation -----------------------------------------------

    #### corrected for unreliability
ideal_statsjedi = viz_diagnostics(stats_jedi, 
                                  aes(exam_three, exam_two), 
                                  fit.lavaan = stats_fit)
  ggsave(filename = "plots/ideal_statsjedi.jpg", ideal_statsjedi)

stats_matrix = visualize(stats_fit, plot="model")
  ggsave(filename = "plots/model_matrix_statsjedi.jpg", stats_matrix)

stats_matrix = visualize(stats_fit)
  ggsave(filename = "plots/all_matrix_statsjedi.jpg", stats_matrix)

    #### not corrected
stats_jedi$latent = lavPredict(stats_fit)
estimated.slope = coef(lm(flexplot::make.formula("exam_two", "latent"), stats_jedi))[2]
corrected.intercept = mean(data[,y]) - estimated.slope * mean(data[,x])
x_new = seq(from=min(stats_jedi$exam_three), to=max(stats_jedi$exam_three), length.out=20)
y_new = corrected.intercept + estimated.slope*x_new
m = data.frame(x=x_new, y=y_new)
reliability_biased_statsjedi = flexplot(exam_two~exam_three, data=stats_jedi) +
  geom_line(data=m, aes(x_new, y_new), col="red")
ggsave(filename = "plots/reliability_biased_statsjedi.jpg", reliability_biased_statsjedi)

ggplot(data=d, aes(x=x1,y=x2)) +
  geom_point() + 
  geom_line(data=m, aes(x1, latent), col="red") +
  geom_smooth(method="lm") + 
  labs(x="Exam One", y="Exam Two")

a = visualize(stats_fit, method="lm", plot = "model")
  ggsave("plots/stats_jedi_model.pdf", a)
a = visualize(stats_fit, method="lm")
  ggsave("plots/stats_jedi_both.pdf", a)