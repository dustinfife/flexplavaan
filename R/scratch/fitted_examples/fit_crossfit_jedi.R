require(lavaan)
require(flexplot)
data(jedi_jedi)
head(jedi_jedi)
model = "
force_score =~ fitness + saber + midichlorian + force_history
jedi_score =~ exam_one + exam_two + exam_three
jedi_score ~ force_score
"
force_fit = cfa(model, jedi_jedi)
summary(force_fit, fit.measures=TRUE, standardized=TRUE)

stats_matrix = visualize(force_fit, plot = "model", subset = 4:7) + 
  theme(strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 12))
ggsave(filename = "plots/model_matrix_jedi.jpg", stats_matrix)

stats_matrix = visualize(force_fit, subset = 4:7) + 
  theme(strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 12))
ggsave(filename = "plots/all_matrix_jedi.jpg", stats_matrix)

## specify model
model_cross = "
force_score =~ fitness + saber + midichlorian + force_history
jedi_score =~ exam_one + exam_two + exam_three + force_history
jedi_score ~ force_score
"
## fit model
force_cross = cfa(model_cross, jedi_jedi)

## visualize it
stats_matrix = visualize(force_fit, force_cross, subset = 4:7, suppress_smooth=T) + 
  theme(strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 12))
ggsave(filename = "plots/comparison_matrix_jedi.jpg", stats_matrix)

### compare fits more globally
data(jedi_jedi)
jedi_jedi[,c("force", "jedi")] = lavPredict(force_cross)

### visualize the measurement models
force = jedi_jedi %>% 
  mutate_at(vars(c(fitness:exam_three)), .funs = scale) %>% 
  gather(measure_force, score_force, fitness:force_history) %>% 
  select(measure_force, score_force, force) 

### visualize the measurement models
jedi = jedi_jedi %>% 
  mutate_at(vars(c(fitness:exam_three)), .funs = scale) %>% 
  gather(measure_jedi, score_jedi, exam_one:exam_three) %>% 
  select(measure_jedi, score_jedi, jedi) 


a = flexplot(force~score_force | measure_force, force, ghost.line=rgb(0,0,1,.5), method="lm", alpha=.3)
b = flexplot(jedi~score_jedi | measure_jedi, jedi, ghost.line=rgb(0,0,1,.5), method="lm", alpha=.1)
measurement_model_jedi = cowplot::plot_grid(a,b, nrow=2)
ggsave(filename = "plots/measurement_jedi.jpg", measurement_model_jedi, width=14, height=10)


ggsave(filename = "plots/factors_jedi.jpg", 
       flexplot(jedi~force, jedi_jedi, ghost.line="red", method="lm"))


lavPredict(force_fit, se="standard")


line_ci = function(x, se){
  n = length(x)
  condse = se*sqrt((1/n) + ((x-mean(x))^2)/sum((x-mean(x))^2))
}
### gather loadings (sorry it's complicated! will make a function soon)
jedi_jedi = jedi_jedi %>% gather(key=model, value=score, force_nocross:jedi_cross) %>% 
  separate(model, c('latent', 'model')) %>% 
  spread(latent, score) %>% 
  mutate(pointsize_force = line_ci(force, se[1]),
         pointsize_jedi = line_ci(jedi, se[2]),
         pointsize_agg = pointsize_force * pointsize_jedi)

### use regular-ole flexplot to visualize latent variables
ggplot(jedi_jedi, aes(force, jedi, size=pointsize_agg, color=model), alpha = .2) +
  geom_point() +
  scale_size_continuous(guide=FALSE) + 
  geom_abline(slope = coef(force_fit)["jedi_score~force_score"], col="blue") + 
  geom_abline(slope = coef(force_cross)["jedi_score~force_score"], col="red")