require(simsem)

popModel <- "
f1 =~ 1*y1 + 0.7*y2 + 0.7*y3
f1 ~~ 1*f1
y1 ~~ 1*y1
y2 ~~ 1*y2
y3 ~~ 1*y3
y2 ~ .75*x
y3 ~ .75*x
"

data <- generate( model = popModel, n = 500 )


analyzeModel <- "
    f1 =~ y1 + y2 + y3
"

summary( modelfit <- cfa( analyzeModel, data = data ) )

require(flexplot)
require(flexplavaan)


visualize( modelfit )






popModel <- "
f1 =~ .5*x1 + .5*x2
f1 ~~ 1*f1
y1 ~~ 1*y1
y2 ~~ 1*y2
y3 ~~ 1*y3
y2 ~ .75*x
y3 ~ .75*x
"

data <- generate( model = popModel, n = 500 )


analyzeModel <- "
    f1 =~ y1 + y2 + y3
"

summary( modelfit <- cfa( analyzeModel, data = data ) )

require(flexplot)
require(flexplavaan)

wellbeing <- "
well =~ posaff + meaning + satis 
"

summary( wellbeingfit <- cfa( wellbeing, data = adult ) )

windows()
visualize(wellbeingfit)
implied_measurement(wellbeingfit)

wellfs <- data.frame( lavPredict(wellbeingfit, append.data = T ) )

welldf <- adult[, c('posaff','meaning','satis') ]
welldf1 <- welldf[complete.cases(welldf),]
welldf1$x <- rep( c(0,1), times = 658 )
welldf1$posaff1 <- NA
welldf1$posaff1[ welldf1$x == 0 ] <- welldf1$posaff[ welldf1$x == 0 ]
welldf1$posaff1[ welldf1$x == 1 ] <- welldf1$posaff[ welldf1$x == 1 ] + rnorm( 658, .5*sd(welldf1$posaff, na.rm = T), .01 )
welldf1$satis1 <- NA
welldf1$satis1[ welldf1$x == 0 ] <- welldf1$satis[ welldf1$x == 0 ]
welldf1$satis1[ welldf1$x == 1 ] <- welldf1$satis[ welldf1$x == 1 ] + rnorm( 658, .5*sd(welldf1$satis, na.rm = T), .01 )


wellbeing1 <- "
well =~ posaff1 + meaning + satis1 
well ~~ 1*well
"

summary( wellbeing1fit <- cfa( wellbeing1, data = welldf1,  ) )

windows()
visualize( wellbeing1fit )



cfa1 <- '
well =~ satis + posaff + meaning
'
summary( cfa1fit <- cfa( cfa1, data = adult  ) )


welldf <- adult[, c('posaff','meaning','satis') ]
welldf1 <- welldf[complete.cases(welldf),]
welldf1$x <- rep( c(0,1), times = 658 )
welldf1$posaff1 <- NA
welldf1$posaff1[ welldf1$x == 0 ] <- welldf1$posaff[ welldf1$x == 0 ]
welldf1$posaff1[ welldf1$x == 1 ] <- welldf1$posaff[ welldf1$x == 1 ] + rnorm( 658, .5*sd(welldf1$posaff, na.rm = T), .01 )
welldf1$satis1 <- NA
welldf1$satis1[ welldf1$x == 0 ] <- welldf1$satis[ welldf1$x == 0 ]
welldf1$satis1[ welldf1$x == 1 ] <- welldf1$satis[ welldf1$x == 1 ] + rnorm( 658, .5*sd(welldf1$satis, na.rm = T), .01 )
