
setwd("C:/RData")
dd <- read.table("C:/RData/AlcBeer8.csv", header=TRUE, sep=",", 
  na.strings="NA", dec=".", strip.white=TRUE)
numSummary(dd[,c("x", "y"), drop=FALSE], statistics=c("mean", "sd", 
  "se(mean)", "IQR", "quantiles", "cv"), quantiles=c(0,.25,.5,.75,1))
with(dd, Hist(x, scale="frequency", breaks="Sturges", col="darkgray"))
with(dd, Hist(y, scale="frequency", breaks="Sturges", col="darkgray"))
Boxplot( ~ y, data=dd, id.method="y")
library(lattice, pos=19)
scatterplot(y~x, reg.line=lm, smooth=TRUE, spread=FALSE, boxplots='xy', 
  span=0.5, ellipse=FALSE, levels=c(.5, .9), jitter=list(x=1, y=1), data=dd)
RegModel.1 <- lm(y~x, data=dd)
summary(RegModel.1)
m1 <- lm(y~x, data=dd)
summary(m1)
confint(m1)
Confint(m1, level=0.95)
Anova(m1, type="III")
anova(m1)

