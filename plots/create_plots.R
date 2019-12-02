require(tidyverse)
test = read.csv("plots/pardoplot.csv", header=FALSE) %>% setNames(c(paste0("Balance", 1:5), paste0("Wobble", 1:5)))

balance = test %>% gather("Time", "Balance", Balance1:Balance4) %>% 
  mutate(Time = factor(Time, unique(Time), labels=c(1:4))) %>% select(Time, Balance)
wobble = test %>% gather("Time", "Wobble", Balance5:Wobble3) %>% 
  mutate(Time = factor(Time, unique(Time), labels=c(1:4))) %>% select(Time, Wobble)

n = cbind(balance, wobble$Wobble) %>% gather("Measure", "Score", 2:3) %>% 
  mutate(Measure = factor(Measure, unique(Measure), labels=c("Balance", "Wobble"))) 

library(extrafont)
loadfonts()
pardo = ggplot(n, aes(Time, Score, group=Measure, col=Measure, linetype=Measure)) +
  geom_jitter() +
  geom_line(data = n %>% group_by(Measure, Time) %>% summarize(Score = mean(Score))) +
  theme_bw() + theme(text=element_text(size=28, family="Moon Flower Bold")) +  
  theme(
    rect = element_rect(fill = "transparent") # all rectangles
  )
ggsave("plots/pardo.jpg", pardo)
