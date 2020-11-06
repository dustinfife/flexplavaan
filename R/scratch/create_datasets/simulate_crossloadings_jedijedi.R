set.seed(223)
require(lavaan) 
require(ggplot2)
n = round(runif(1, 500, 800))

force_score = rnorm(n)
jedi_score = .5*force_score + rnorm(n, sqrt(1-.5^2))

# create three exams
# jedi counsel hired a psychometrician to develop a batter of tests to identify candidate jedis
fitness = 50 + .2*(11)*force_score + rnorm(n,0, sqrt(1-.6^2)*11) ### minutes required to complete obstacle course
saber = 0 + .3*(15)*force_score + rnorm(n,0, sqrt(1-.7^2)*15)  ### hits on opponent vs. hits on you
midichlorian = 200 + .3*(50)*force_score + rnorm(n,0, sqrt(1-.8^2)*50)
force_history = 70 + .5*(15)*force_score + .5*(15)*jedi_score + rnorm(n,0, sqrt(1-.8^2)*15)

exam_one = 70 + .5*(15)*jedi_score + rnorm(n,0, sqrt(1-.6^2)*15)
exam_two = 70 + .3*(15)*jedi_score + rnorm(n,0, sqrt(1-.7^2)*15)
exam_three = 70 + .2*(15)*jedi_score + rnorm(n,0, sqrt(1-.8^2)*15)

jedi_jedi = data.frame(fitness = fitness, saber = saber, midichlorian=midichlorian, force_history,
                        exam_one=exam_one, exam_two=exam_two, exam_three=exam_three)

usethis::use_data(jedi_jedi, overwrite = TRUE)