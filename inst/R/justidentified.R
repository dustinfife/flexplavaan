require(simsem)

popModel <- "
f1 =~ .7*y1 + 0.7*y2 + 0.7*y3
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

welldf <- adult[, c('PositiveAffect18_85','Meaning18_85','LifeSatisfaction18_85') ]
welldf1 <- welldf[complete.cases(welldf),]
welldf1$x <- rep( c(0,1), times = 663 )
welldf1$posaff1 <- NA
welldf1$posaff1[ welldf1$x == 0 ] <- welldf1$PositiveAffect18_85[ welldf1$x == 0 ]
welldf1$posaff1[ welldf1$x == 1 ] <- welldf1$PositiveAffect18_85[ welldf1$x == 1 ] + rnorm( 663, 1.2*sd(welldf1$PositiveAffect18_85, na.rm = T), .01 )
welldf1$meaning1 <- NA
welldf1$meaning1[ welldf1$x == 0 ] <- welldf1$Meaning18_85[ welldf1$x == 0 ]
welldf1$meaning1[ welldf1$x == 1 ] <- welldf1$Meaning18_85[ welldf1$x == 1 ] + rnorm( 663, 1.2*sd(welldf1$Meaning18_85, na.rm = T), .01 )


wellbeing1corr <- "
well =~ posaff1 + meaning1 + LifeSatisfaction18_85 
posaff1 + meaning1 ~ x
"

summary( wellbeing1corrfit <- cfa( wellbeing1corr, data = welldf1, auto.fix.first = F, std.lv =T  ) )

wellbeing1incorr <- "
well =~ posaff1 + meaning1 + LifeSatisfaction18_85 
"

summary( wellbeing1incorrfit <- cfa( wellbeing1incorr, data = welldf1, auto.fix.first = F, std.lv =T  ) )

windows()
visualize( wellbeing1incorrfit )



wellbeing <- "
well =~ Meaning18_85 + LifeSatisfaction18_85 
well ~ PositiveAffect18_85
"

summary( wellbeingfit <- cfa( wellbeing, data = welldf1, auto.fix.first = F, std.lv =T  ) )

windows()
visualize( wellbeing1incorrfit )





cfa1 <- '
well =~ satis + posaff + meaning
'
summary( cfa1fit <- cfa( cfa1, data = adult, auto.fix.first = F, std.lv =T   ) )


welldf <- adult[, c('posaff','meaning','satis') ]
welldf1 <- welldf[complete.cases(welldf),]
welldf1$x <- rep( c(0,1), times = 658 )
welldf1$posaff1 <- NA
welldf1$posaff1[ welldf1$x == 0 ] <- welldf1$posaff[ welldf1$x == 0 ]
welldf1$posaff1[ welldf1$x == 1 ] <- welldf1$posaff[ welldf1$x == 1 ] + rnorm( 658, .5*sd(welldf1$posaff, na.rm = T), .01 )
welldf1$satis1 <- NA
welldf1$satis1[ welldf1$x == 0 ] <- welldf1$satis[ welldf1$x == 0 ]
welldf1$satis1[ welldf1$x == 1 ] <- welldf1$satis[ welldf1$x == 1 ] + rnorm( 658, .5*sd(welldf1$satis, na.rm = T), .01 )






popModel <- "
xi =~ .8*x1 + .8*x2
xi ~~ 1*xi
eta =~ .8*y1 + .8*y2
eta ~~ 1*eta
z ~~ 1*z
eta ~ .5*z
xi ~ .5*z
eta ~ .5*xi
"

data <- generate( model = popModel, n = 500 )



m <- '
xi =~ x1 + x2
y1 ~ xi

'

summary( mfit <- sem( m, data = data ) )


