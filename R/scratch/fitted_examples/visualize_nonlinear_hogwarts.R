  #### prep the data
require(tidyverse)
require(flexplot)
data("hogwarts_survival")
data = hogwarts_survival


hogwarts_nonlinear = readRDS(file="data/hogwarts_summary.rds")

visualize.runjags(hogwarts_nonlinear, data[,1:3], which.latent=c(1,1,1))
visualize.runjags(hogwarts_nonlinear, data[,4:6], which.latent=c(2,2,2))

