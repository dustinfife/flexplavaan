require(lavaan)
require(flexplot)
require(tidyverse)
data(jedi_jedi)


# specify the models ------------------------------------------------------

model = "
force_score =~ fitness + saber + midichlorian + force_history
jedi_score =~ exam_one + exam_two + exam_three
jedi_score ~ force_score
"
## specify model
model_cross = "
force_score =~ fitness + saber + midichlorian + force_history
jedi_score =~ exam_one + exam_two + exam_three + force_history
jedi_score ~ force_score
"


# Fit the models ----------------------------------------------------------

force_fit = cfa(model, jedi_jedi)
force_cross = cfa(model_cross, jedi_jedi)
  #summary(force_fit, fit.measures=TRUE, standardized=TRUE)
  #summary(force_cross, fit.measures=TRUE, standardized=TRUE)



# Visualize the models ----------------------------------------------------
visualize(force_fit, force_cross, subset = 4:7, suppress_smooth=T)


# Visualize Measurement Model ---------------------------------------------

### compare fits more globally
jedi_jedi[,c("force", "jedi")] = lavPredict(force_cross)

### visualize the measurement models
force = jedi_jedi %>% 
  mutate_at(vars(c(fitness:exam_three)), .funs = scale) %>% 
  gather(measure_force, score_force, fitness:force_history) %>% 
  select(measure_force, score_force, force) 

jedi = jedi_jedi %>% 
  mutate_at(vars(c(fitness:exam_three)), .funs = scale) %>% 
  gather(measure_jedi, score_jedi, exam_one:exam_three) %>% 
  select(measure_jedi, score_jedi, jedi) 


a = flexplot(force~score_force | measure_force, force, ghost.line=rgb(0,0,1,.5), method="lm", alpha=.3)
b = flexplot(jedi~score_jedi | measure_jedi, jedi, ghost.line=rgb(0,0,1,.5), method="lm", alpha=.1)
measurement_model_jedi = cowplot::plot_grid(a,b, nrow=2)
ggsave(filename = "plots/measurement_jedi.jpg", measurement_model_jedi, width=14, height=10)


### plot the latent variables
flexplot(jedi~force, jedi_jedi, ghost.line="red", method="lm")













# Create graphics for presentation ----------------------------------------


stats_matrix = visualize(force_fit, plot = "model", subset = 4:7) + 
  theme(strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 12))
ggsave(filename = "plots/model_matrix_jedi.jpg", stats_matrix)

stats_matrix = visualize(force_fit, subset = 4:7) + 
  theme(strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 12))
ggsave(filename = "plots/all_matrix_jedi.jpg", stats_matrix)

stats_matrix = visualize(force_fit, force_cross, subset = 4:7, suppress_smooth=T) + 
  theme(strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 12))
ggsave(filename = "plots/comparison_matrix_jedi.jpg", stats_matrix)
