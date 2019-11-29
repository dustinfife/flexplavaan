set.seed(223)
require(lavaan) 
require(ggplot2)
n = round(runif(1, 500, 1000))
n = 5000

  ## define latent variables
wizard_skills = rnorm(n)
darkness = .2*wizard_skills + rnorm(n, 0, sqrt(1-.2^2))
  survived_link = exp(2 + 2*wizard_skills + 1.2*darkness)
survived = survived_link/(1+survived_link)
  
  ### make logistic function
logistic = function(x, max, slope, intercept){
  max/(1 + exp(-1*slope*(x-intercept)))
}

  ### outcome: who survives hogwarts final battle

  ### predictors: hogwarts exams (/100), dark arts (polarizing)
potions = 70 + .5*(15)*wizard_skills + rnorm(n, 0, sqrt(1-.5^2) * 15)
darkarts = 70 + .9*(15)*wizard_skills + rnorm(n, 0, sqrt(1-.9^2) * 15)
spells = 70 + .8*(15)*wizard_skills + rnorm(n, 0, sqrt(1-.8^2) * 15)

  ### darkness
muggle_opinion = logistic(darkness, 10, 1, -1) + rnorm(n, 0, .5)
darklord = logistic(darkness, 10, 1, -1) + rnorm(n, 0, .5)
plot(darkness, muggle_opinion)

# create three indicators of magic
x1 = 20 + .8*4*latent + rnorm(n,0, sqrt(1-.8^2)*4)
mx = 6  ### maximum + of occurances
mid = 1.5 ### midpoint (point at which half of max is reached)
slope = 4 ### rate of increase per sd increase (at mid point)
x2 = round(mx/(1 + exp(-slope*(latent-mid)))  + rpois(n, 0))
b0 = 1-6*min(latent) 
x3 = b0 + .9*6*latent + (rnorm(n, 0, sqrt(1-.9^2)*6))
x3[x3<0] = 0
plot(x1, x2)

y1 = 70 + .9*15*latent2 + rnorm(n,0, sqrt(1-.9^2)*15)
y2 = 70 + .6*15*latent2 + rnorm(n,0, sqrt(1-.6^2)*15)
y3 = 70 + .45*15*latent2 + rnorm(n,0, sqrt(1-.45^2)*15)

mugglevwizard = cut(latent, c(-Inf, 1.5, Inf), labels = c("muggle", "witch_wizard"))

gender = sample(c("boy", "girl"), size = 500, replace=T)

mugglevwizard = data.frame(strange=x1, relatives=x2, wingardium=x3, darkarts=y1, potions=y2, history=y3, mugglevwizard=mugglevwizard,
                           gender = gender)
require(dplyr)
mugglevwizard = mugglevwizard %>% mutate(mugglevwizard = ifelse(gender=="boy", "wizard", "witch"))  %>% select(-gender) %>% mutate_if(is.numeric, round)
usethis::use_data(mugglevwizard, overwrite = TRUE)





force_score = rnorm(n)
jedi_score = .5*force_score + rnorm(n, sqrt(1-.5^2))

# create three exams
# jedi counsel hired a psychometrician to develop a batter of tests to identify candidate jedis
fitness = 50 + .6*(11)*force_score + rnorm(n,0, sqrt(1-.6^2)*11) ### minutes required to complete obstacle course
saber = 0 + .7*(15)*force_score + rnorm(n,0, sqrt(1-.7^2)*15)  ### hits on opponent vs. hits on you
midichlorian = 200 + .8*(50)*force_score + rnorm(n,0, sqrt(1-.8^2)*50)
force_history = 70 + .3*(15)*force_score + .4*(15)*jedi_score + rnorm(n,0, sqrt(1-.8^2)*15)

exam_one = 70 + .6*(15)*jedi_score + rnorm(n,0, sqrt(1-.6^2)*15)
exam_two = 70 + .7*(15)*jedi_score + rnorm(n,0, sqrt(1-.7^2)*15)
exam_three = 70 + .8*(15)*jedi_score + rnorm(n,0, sqrt(1-.8^2)*15)

jedi_jedi = data.frame(fitness = fitness, saber = saber, midichlorian=midichlorian, force_history,
                        exam_one=exam_one, exam_two=exam_two, exam_three=exam_three)

usethis::use_data(jedi_jedi, overwrite = TRUE)