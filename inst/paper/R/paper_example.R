require(lavaan) 
require(flexplavaan)
require(ggplot2)
set.seed(2323)
n = round(runif(1, 2000, 2500))

latent = rnorm(n)
latent2 = rnorm(n)

# create three exams
exam_one = .7*latent + rnorm(n,0, sqrt(1-.7^2))
exam_two = .7*latent + rnorm(n,0, sqrt(1-.5^2))
exam_three = .2*latent + .7*latent2 + rnorm(n,0, sqrt(1-.5^2))
exam_four = .6*latent2 + rnorm(n,0, sqrt(1-.5^2))
exam_five = .6*latent2 + rnorm(n,0, sqrt(1-.5^2))


d = data.frame(x1=exam_one, x2=exam_two, x3=exam_three, x4=exam_four, x5=exam_five)
model = "Latent =~ x1 + x2 + x3
Latent2 =~ x4 + x5"
stats_fit = cfa(model, d)
standardizedSolution(stats_fit)
lavInspect(stats_fit, "cor.ov")
a = visualize(stats_fit, plot="model")
cross_loading = a+theme_bw()
ggsave(cross_loading, file="plots/crossloading.jpg")
cross_loading_diagnostics = visualize(stats_fit)
ggsave(cross_loading_diagnostics, file="plots/crossloading_ddp.jpg")

latent = rnorm(n)
latent2 = rnorm(n)

# create three exams
exam_one = .5*latent + rnorm(n,0, sqrt(1-.5^2))
exam_two = .5*latent + rnorm(n,0, sqrt(1-.5^2))
exam_three = .6*latent2 + rnorm(n,0, sqrt(1-.5^2))

cor(exam_one, exam_three)

d = data.frame(x1=exam_one, x2=exam_two, x3=exam_three)
model = "Latent =~ x1 + x2 + x3"
stats_fit = cfa(model, d)
measurement = visualize(stats_fit, plot="model", method="lm")
fits = fitMeasures(stats_fit)
standardizedSolution(stats_fit)
ggsave(measurement, file="plots/poorfit.jpg")
save(fits, file="data/crossloading_plot.Rdata")



















x1 = 70 + .6*(15)*latent + rnorm(n,0, sqrt(1-.6^2)*15)
x2 = 70 + .7*(15)*latent + rnorm(n,0, sqrt(1-.6^2)*15)
x3 = 70 + .2*(15)*latent + .4*latent2 + rnorm(n,0, sqrt(1-.6^2)*15)
x4 = 70 + .6*(15)*latent2 + rnorm(n,0, sqrt(1-.6^2)*15)
x5 = 70 + .5*(15)*latent2 + rnorm(n,0, sqrt(1-.6^2)*15)
x6 = 70 + .6*(15)*latent2 + rnorm(n,0, sqrt(1-.6^2)*15)
d = data.frame(x1=x1, x2=x2, x3=x3, x4=x4, x5=x5, x6=x6)
model = "Latent1 =~ x1 + x2 + x3
Latent2 =~ x4 + x5 + x6"
stats_fit = cfa(model, d)
#standardizedSolution(stats_fit)
b = visualize(stats_fit, plot="model", subset=c(2,3,4))
b
library(gridExtra)    
grid.arrange(grobs=c(a,b), nrow=2)
cowplot::plot_grid(
  ggmatrix_gtable(a),
  ggmatrix_gtable(b),
  nrow = 1
)

my_grobs = lapply(list(a,b), ggplotGrob)

cowplot::plot_grid(plotlist = list(a,b))
plot_list = 

flexplot(x3~x2, data=d)


visualize.flexplavaan
summary(stats_fit)
lavInspect(stats_fit, what="cor.ov")

?`fitted,lavaan-method`
viz_diagnostics(d, 
                aes(x2, x3), 
                fit.lavaan = stats_fit)+
  theme_bw(base_family="Baskerville", base_size=22) + 
  labs(x="Exam Three", y="Exam Two")


#### corrected for unreliability
ideal_statsjedi = viz_diagnostics(stats_jedi, 
                                  aes(exam_three, exam_two), 
                                  fit.lavaan = stats_fit)+
  theme_bw(base_family="Baskerville", base_size=22) + 
  labs(x="Exam Three", y="Exam Two")
