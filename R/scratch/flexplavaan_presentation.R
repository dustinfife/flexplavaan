require(lavaan)
#devtools::install_github("dustinfife/flexplavaan")
require(flexplavaan)

# look at the data
head(jedi_jedi)
# data that generated the image vs. model to be fit

# specify the models ------------------------------------------------------

model = "
Force =~ fitness + saber + midichlorian + force_history
Jedi =~ exam_one + exam_two + exam_three
Jedi ~ Force
"

# Fit the models ----------------------------------------------------------
force_fit = sem(model, jedi_jedi)
summary(force_fit, fit.measures=TRUE)
# all indices say it's a good model!

residual_plots(force_fit)

visualize(force_fit, subset=1:4)
# there's nonlinearity among exams/force_history

implied_measurement(force_fit)
# exams are all nonlinear with both LVs!



# respecify the model -----------------------------------------------------
# this would be far easier if we were Bayesian :) (coming soon...)
# this model looks like this....
d = jedi_jedi %>% mutate(
  fit_sq = (scale(fitness))^2,
  saber_sq = (scale(saber))^2,
  midi_sq = (scale(midichlorian))^2
)
model = "
Force =~ fitness + saber + midichlorian + force_history
force_sq =~ fit_sq + saber_sq + midi_sq
Jedi =~ exam_one + exam_two + exam_three + force_history 
Jedi ~ Force + force_sq
Force ~~ force_sq
"
nonlinear = sem(model, d)
summary(nonlinear, fit.measures=T)
  # it's improved!

residual_plots(nonlinear)
  # all much closer, but we have squared "variables"

visualize(nonlinear, subset=1:4)
# well that's just weird :)

implied_measurement(nonlinear, latent="Force")
# that's improved!
implied_measurement(nonlinear, latent="force_sq")
# a bit wonky, but that's to be expected
implied_measurement(nonlinear, latent="Jedi")
# also fairly close, but imperfect

visualize(nonlinear, plot="latent", 
          formula = Jedi~Force | force_sq)



