require(fifer)
set.seed(1212)
    ## hogwarts success, as a function of IQ, relatives with talent, house, social adaptation
hog_iq = make.data(.4, means=c(70, 100), sds = c(15, 15), n=376)
relatives = rpois(376, 5)
house = sample(c("ravenclaw", "hufflepuff", "slytherin", "gryffindor"), size=376, replace=T)
muggle = rep("no", times=376)
notmug = relatives==0
muggle[notmug] = sample(c("yes", "no"), size=sum(notmug), replace=T)
social = rnorm(376, 20, 6)

hogwarts = data.frame(success = hog_iq[,1], iq = hog_iq[,2], relatives = relatives, house=house, muggle=muggle, social = social)
flexplot(success~iq, data=hogwarts)
head(model.matrix(success~iq + relatives + house + muggle + social + house*iq + I(social^2), data=hogwarts))
coefs = matrix(c(0, 1, 19.5, .3, -.2, 188.5, 3, 1.8*(sd(hogwarts$success)/sd(hogwarts$social)), -.1*(sd(hogwarts$success)/sd(hogwarts$social)), 
                 .2, -1.5, -3.912), nrow=1)
hogwarts$success = hogwarts$success + t(coefs %*% t(model.matrix(success~iq + relatives + house + muggle + social + house*iq + I(social^2), data=hogwarts)) )
flexplot(success~iq | house, data=hogwarts)
flexplot(success~relatives | house, data=hogwarts)
flexplot(success~iq , data=hogwarts)
flexplot(success~social + house , data=hogwarts)
hogwarts$success = rescale(hogwarts$success, 60, 15)
hogwarts = hogwarts %>% mutate_if(is.numeric, round)
write.csv(hogwarts, "data/hogwarts_students.csv", row.names=F)
