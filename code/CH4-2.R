setwd("C:/Users/User/Documents/Regression_summar/")
library(data.table)
library(dplyr)

car2013_0 <- fread("cars2013test.csv")
car2013 <- car2013_0 %>% select(price, mpgcitycity, hpower, CC, weight)

###################
####描述性統計#####
###################
library(RcmdrMisc)
summary(car2013)
numSummary(car2013, statistics = c("mean", "sd", "se(mean)", "IQR", "quantiles", "cv"), quantiles=c(0,.25,.5,.75,1))

################
###複回歸模型###
################
MA <- lm(price ~ mpgcitycity + hpower + CC + weight, data = car2013)
summary(MA)

library(car)
options(contrasts = c("contr.sum", "contr.poly"))
# Type I test
anova(MA)
# Type III test
Anova(MA, type = "III")

# Nested model comparison
MB <- lm(price ~ mpgcitycity + hpower, data = car2013)
summary(MB)

anova(MA, MB, test = "F")

# 反過來做
MA_t <- lm(price ~ hpower + CC + mpgcitycity + weight, data = car2013)
# Type I test
anova(MA_t)
# Type III test
Anova(MA_t, type = "III")
