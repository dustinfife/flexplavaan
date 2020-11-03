rm(list=ls())

## Working directory 'flexplavaan/exampleDataSets'

library( lavaan )
library( MIIVsem )
library(devtools)
# install_github("dustinfife/flexplavaan")
# install_github("dustinfife/flexplot")
library( flexplavaan )

load("Bollen1989/bollen1989.RData")
load("College Transition Study/ctsrdata.RData")


## Bollen Political Democracy Model with correlations among disturbances for repeated indicators
## https://tutorials.methodsconsultants.com/posts/a-running-cfa-and-sem-example/

democ1 <- '
Eta1 =~ y1 + y2 + y3 + y4
Eta2 =~ y5 + y6 + y7 + y8
Xi1 =~ x1 + x2 + x3
Eta1 ~ Xi1
Eta2 ~ Xi1
Eta2 ~ Eta1
y1 ~~ y5
y2 ~~ y4
y2 ~~ y6
y3 ~~ y7
y4 ~~ y8
y6 ~~ y8
'

summary( bollen1 <- sem( democ1, data = bollen1989a ) )
visualize( bollen1 )


## Bollen model but remove indicator disturbance correlations
democ2 <- '
Eta1 =~ y1 + y2 + y3 + y4
Eta2 =~ y5 + y6 + y7 + y8
Xi1 =~ x1 + x2 + x3
Eta1 ~ Xi1
Eta2 ~ Xi1
Eta2 ~ Eta1
#y1 ~~ y5
#y2 ~~ y4
#y2 ~~ y6
#y3 ~~ y7
#y4 ~~ y8
#y6 ~~ y8
'

summary( bollen2 <- sem( democ2, data = bollen1989a ) ) 
visualize( bollen2 )



## College Transition Study Piecewise Growth Model

ctsr1 <- ctsr[ , c('id','assess','phqcent','psscent','freshmen','transon','transoff',) ]
ctsr1wide <- reshape( ctsr1, v.names = c('phqcent','psscent'), timevar = 'assess', idvar = 'id',
                      direction = 'wide', sep = '')


##############################
## Perceived stress measured during each month of students' first semester at large university (psscent2 - psscent4)
## and also one month prior (psscent1)
## Compares first-time students (freshmen) to new transfer students living on and off campus -- freshmen are the reference group
## transon compares on-campus transfers to freshmen
## transoff compares off-campus transfers to freshmen
## Should use a piecewise growth model to capture non-constant change over time -- likelihood ratio test favors piecewise growth
## Will flexplavaan give us clues that a simple linear growth model is inadequate?
##############################

## Piecewise growth of stress over time
growth1 <- '
i =~ 1*psscent1 + 1*psscent2 + 1*psscent3 + 1*psscent4 + 1*psscent5
s1 =~ 0*psscent1 + 1*psscent2 + 2*psscent3 + 2*psscent4 + 2*psscent5
s2 =~ 0*psscent1 + 0*psscent2 + 0*psscent3 + 1*psscent4 + 2*psscent5
i + s1 + s2 ~ transon + transoff
'

summary( growth1fit <- growth( growth1, data = ctsr1wide ) )
visualize( growth1fit )

pred <- data.frame( predict( growth1fit ) )
with( pred, plot( s1, s2 ) )


## Piecewise growth of stress over time
growth2 <- '
i =~ 1*psscent1 + 1*psscent2 + 1*psscent3 + 1*psscent4 + 1*psscent5
s =~ 0*psscent1 + 1*psscent2 + 2*psscent3 + 3*psscent4 + 4*psscent5
i + s ~ transon + transoff
'

summary( growth2fit <- growth( growth2, data = ctsr1wide ) )
visualize( growth2fit )

( vis2 <- visualize( growth1fit, growth2fit, subset = 1:3 ) )

## Piecewise growth -- assume constant disturbances over time
growth3 <- '
i =~ 1*psscent1 + 1*psscent2 + 1*psscent3 + 1*psscent4 + 1*psscent5
s1 =~ 0*psscent1 + 1*psscent2 + 2*psscent3 + 2*psscent4 + 2*psscent5
s2 =~ 0*psscent1 + 0*psscent2 + 0*psscent3 + 1*psscent4 + 2*psscent5
i + s1 + s2 ~ transon + transoff
psscent1 ~~ e*psscent1
psscent2 ~~ e*psscent2
psscent3 ~~ e*psscent3
psscent4 ~~ e*psscent4
psscent5 ~~ e*psscent5
'

summary( growth3fit <- growth( growth3, data = ctsr1wide ) )
windows()
visualize( growth3fit )



###########################################################
## Evaluate factor structure of Campus Connectedness Scale
## using the CTSR data set at time 2
###########################################################

scs <- ctsr[ ctsr$assess == 2, c(1:3, 50:63)] 

## Unidimensional factor model 
uni <- '
f =~ scs1+scs2+scs3+scs4+scs5+scs6+scs7+scs8+scs9+scs10+scs11+scs12+scs13+scs14
'

summary( uni.fit <- cfa( uni, estimator = 'mlr', data = scs ), fit.measures = T )

visualize( uni.fit )

## Method factor bifactor model accounting for positive & negative wording 
method <- '
f =~ scs1+scs2+scs3+scs4+scs5+scs6+scs7+scs8+scs9+scs10+scs11+scs12+scs13+scs14
pos =~ 1*scs1+1*scs3+1*scs4+1*scs7+1*scs9+1*scs12
neg =~ 1*scs2+1*scs5+1*scs6+1*scs8+1*scs10+1*scs11+1*scs13+1*scs14
f ~~ 0*pos + 0*neg
pos ~~ 0*neg
'

summary( method.fit <- cfa( method, estimator = 'mlr', data = scs ), fit.measures = T )

visualize(uni.fit, method.fit, subset = 1:5 )



## Method factor bifactor model accounting for positive & negative wording 
method.ord <- '
f =~ scs1+scs2+scs3+scs4+scs5+scs6+scs7+scs8+scs9+scs10+scs11+scs12+scs13+scs14
pos =~ 1*scs1+1*scs3+1*scs4+1*scs7+1*scs9+1*scs12
neg =~ 1*scs2+1*scs5+1*scs6+1*scs8+1*scs10+1*scs11+1*scs13+1*scs14
f ~~ 0*pos + 0*neg
pos ~~ 0*neg
'

summary( method.ord.fit <- cfa( method, estimator = 'wlsmv', data = scs,
                                ordered = c("scs1","scs2","scs3","scs4","scs5",
                                "scs6","scs7","scs8","scs9","scs10","scs11",
                                "scs12","scs13","scs14") ), 
         fit.measures = T ) 

visualize(uni.fit, method.ord.fit, subset = 1:5 )


###########################################################
## Evaluate factor structure of Satisfaction With Life Scale
## using the CTSR data set at time 1
###########################################################

swls <- ctsr[ ctsr$assess == 2, c(1:3, 74:78)] 

## Unidimensional factor model 
swls.uni <- '
f =~ swls1+swls2+swls3+swls4+swls5
'

summary( swls.uni.fit <- cfa( swls.uni, estimator = 'mlr', data = swls ), fit.measures = T )

summary( swls.uni.fit.ml <- cfa( swls.uni, estimator = 'ml', data = swls ), fit.measures = T )


## Unidimensional factor model 
swls.uni.ord <- '
f =~ swls1+swls2+swls3+swls4+swls5
'

summary( swls.uni.fit <- cfa( swls.uni, estimator = 'mlr', data = swls ), fit.measures = T )




swls.uni.plot <- visualize( swls.uni.fit )

## Unidimensional factor model -- drop item 5
swls.uni2 <- '
f =~ swls1+swls2+swls3+swls4
'

summary( swls.uni.fit2 <- cfa( swls.uni2, estimator = 'mlr', data = swls ), fit.measures = T )

visualize( swls.uni.fit2 )

require(tidyverse)


swls$swls1t <- swls$swls1^1.5
swls$swls2t <- swls$swls2^1.5
swls$swls3t <- swls$swls3^1.5
swls$swls4t <- swls$swls4^1.5
swls$swls5t <- swls$swls5^1.5


d <- swls %>% select( swls1,swls2,swls3,swls4,swls5 ) %>% mutate_all( .,funs = function(x) x^1.5  )


library(MplusAutomation)


prepareMplusData(swls, file='swls.dat')



## Unidimensional factor model 
swls.uni.t <- '
f =~ swls1t+swls2t+swls3t+swls4t+swls5t
'

summary( swls.uni.fit.t <- cfa( swls.uni.t, estimator = 'mlr', data = swls ), fit.measures = T )