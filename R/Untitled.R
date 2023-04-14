# object = readRDS(file="data/hogwarts_nonlinear.rds")
# hogwarts_summary = readRDS(file="data/hogwarts_summary.rds")
# data(hogwarts_survival)
# data = hogwarts_survival
# which.latent = c(1,1,1,2,2,2)
# mapping = aes(potions, darkarts)
# visualize.runjags(object, data, summary_bayesian = hogwarts_summary, which.latent=c(1,1,1,2,2,2))
# viz_diagnostics_mcmc(data, mapping, latents=factor.scores, plot="disturbance")
# 
# 
# 



# head(data)
# 
# 
# strsplit()
# 
# ?strsplit
# 
# test = object$data
# 
# grep("([0-9]+)", test)
# strsplit(test, )
# 
# 
# 
# %>%
#   matrix( ncol=2, byrow=F) 
# 
# head(k[,1])
# %>% 
#   
# data.frame(test)
# 
# a = c(list("a", paste0(1:5)), list("b", paste0(1:5)))
# a
# 
# gsub("[a-z]+[_]?[a-z]+")
# 
# # Create the vector of lists
# a = c(list("a", paste0(1:5)), list("b", paste0(6:10)))
# 
# # Convert the vector of lists to a data frame
# df <- data.frame(a)
# df <- setNames(df, c("col1", "col2"))
# 
# 
# data.frame(v1 = 'hello-world;1|(good)night world;2|') %>% 
#   mutate(v1 = strsplit(as.character(v1), '\\|')) %>% 
#   unnest(v1) %>% 
#   separate(v1, into = c('v1', 'v2'), sep = ';')
# 
# str(test)
# data.frame(v1 = test) %>%
#   mutate(v1 = strsplit(as.character(v1), "\n"))
# extract(object, "data")
