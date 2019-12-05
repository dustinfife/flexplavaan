set.seed(223)
require(lavaan) 
require(ggplot2)
n = round(runif(1, 500, 1000))

  ### make functions
logistic = function(x, max, slope, intercept){
  max/(1 + exp(-1*slope*(x-intercept)))
}
ceiling_floor = function(x, max=100, min=0){
  x[x>max] = max
  x[x<min] = min
  x
}

  ## define latent variables (knowledge, skills, abilities)
magic_knowledge = rnorm(n)
magic_skills = .3*magic_knowledge + rnorm(n, 0, sqrt(1-.3^2))
survived_link = exp(2 + 2*magic_skills + 1.2*magic_knowledge)
survived = survived_link/(1+survived_link)
survived = round(survived); #survived = factor(survived, levels=c(0,1), labels=c("no", "yes"), ordered=T)
#plot(magic_knowledge, survived)
#plot(magic_skills, survived)

  ### knowledge questions
potions = 70 + .7*(15)*magic_knowledge + rnorm(n, 0, sqrt(1-.7^2) * 15); potions =potions%>% ceiling_floor
history = 70 + .8*(15)*magic_knowledge + rnorm(n, 0, sqrt(1-.8^2) * 15); history =history%>% ceiling_floor
herbology = 70 + .5*(15)*magic_knowledge + rnorm(n, 0, sqrt(1-.5^2) * 15); herbology =herbology%>% ceiling_floor

  ### abilities
darkarts = logistic(magic_skills, 80, 2.5, -1.250) + rnorm(n, 0, 7); darkarts = darkarts%>% ceiling_floor
flying = 70 + .9*(15)*magic_skills + rnorm(n, 0, sqrt(1-.9^2) * 15); flying =  flying %>% ceiling_floor
spells = 70 + .5*(15)*magic_skills + rnorm(n, 0, sqrt(1-.5^2) * 15); spells= spells%>% ceiling_floor
plot(flying, darkarts)

hogwarts_survival = data.frame(potions = potions, history=history, herbology = herbology, 
                               darkarts = darkarts, flying = flying, spells = spells,
                               survived = survived,
                               magic_knowledge=magic_knowledge, magic_skills = magic_skills)

summary(hogwarts_survival)
usethis::use_data(hogwarts_survival, overwrite = TRUE)

