setwd("~/Regression_summar")
library(data.table)
library(dplyr)

USA0 <- fread("USAlumniDonation.csv")
USA <- USA0 %>% select(AGR, grad, under, SFR)

###################
####描述性統計#####
###################
library(RcmdrMisc)
summary(USA)
numSummary(USA, statistics = c("mean", "sd", "se(mean)", "IQR", "quantiles", "cv"), quantiles=c(0,.25,.5,.75,1))

################
###複回歸模型###
################
MA <- lm(AGR ~ grad + under + under + SFR, data = USA)
summary(MA)

library(car)
options(contrasts = c("contr.sum", "contr.poly"))
# Nested model comparison M0
M0 <- lm(AGR ~ 1, data = USA)
anova(MA, M0, test = "F")

# Type I test
anova(MA)
# Type III test
Anova(MA, type = "III")

# Nested model comparison MB
MB <- lm(AGR ~ grad, data = USA)
anova(MA, MB, test = "F")
