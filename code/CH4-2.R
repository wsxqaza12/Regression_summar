setwd("~/Regression_summar")
library(data.table)
library(dplyr)

car2013_0 <- fread("cars2013test.csv")
car2013 <- car2013_0 %>% select(price, mpgcitycity, hpower, CC, weight)

###################
####描述性統計#####
###################
library(RcmdrMisc)

numS_dataframe <- function(x){
  library(plyr)
  library(dplyr)
  temp <- numSummary(x, statistics = c("mean", "sd", "se(mean)", "IQR", "quantiles", "cv"), quantiles=c(0,.25,.5,.75,1))
  df <- ldply (temp, data.frame)
  sel <- df[df$.id == "table", ]
  sel <- sel %>% select(-.id ,- X..i..)
  colnames(sel) <- c("mean", "sd", "se(mean)", "IQR", "cv", "0%","25%","50%", "75%", "100%")
  rownames(sel) <- names(x)
  sel
}

numS_dataframe(car2013)


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
