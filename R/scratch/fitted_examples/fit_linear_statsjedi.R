require(lavaan)
require(flexplot)
data(stats_jedi)
head(stats_jedi)
model = "stats_jedi =~ exam_one + exam_two + exam_three"
stats_fit = cfa(model, stats_jedi)
summary(stats_fit, fit.measures=TRUE)
visualize(stats_fit, method="lm")
