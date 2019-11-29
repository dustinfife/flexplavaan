require(lavaan)
require(flexplot)
data(jedi_jedi)
head(jedi_jedi)
model = "
force_score =~ fitness + saber + midichlorian + force_history
jedi_score =~ exam_one + exam_two + exam_three
"
force_fit = cfa(model, jedi_jedi)
summary(force_fit, fit.measures=TRUE)
visualize(force_fit, method="lm", subset = 1:4)
visualize(force_fit, method="lm", subset = 4:7)

