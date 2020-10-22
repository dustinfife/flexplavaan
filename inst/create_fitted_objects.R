require(lavaan)
data("correct_small")

model_1 = "
f1 =~ x1 + x2 + x3
f2 =~ y1 + y2 + y3
f1 ~ f2
"
model_2 = "
f1 =~ x1 + x2 + x3
f2 =~ x3 + y1 + y2 + y3
f1 ~ f2
"

fit_twofactor = cfa(model_1, data=correct_small)
usethis::use_data(fit_twofactor)  
