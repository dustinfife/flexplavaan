require(lavaan) 
require(ggplot2)
set.seed(2323)
n = round(runif(1, 300, 1000))

latent = rnorm(n)

# create three exams
exam_one = 70 + .6*(15)*latent + rnorm(n,0, sqrt(1-.6^2)*15)
exam_two = 70 + .7*(15)*latent + rnorm(n,0, sqrt(1-.7^2)*15)
exam_three = 70 + .8*(15)*latent + rnorm(n,0, sqrt(1-.8^2)*15)
stats_jedi = data.frame(exam_one=exam_one, exam_two=exam_two, exam_three=exam_three)

usethis::use_data(stats_jedi, overwrite = TRUE)