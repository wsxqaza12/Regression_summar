setwd("~/Reg")
library(data.table)
library(dplyr)
library(showtext)
library(GGally)

cheese1 <- fread("cheese.csv")
cheese <- cheese1 %>% select(taste, H2S, Lactic)

###################
####描述性統計#####
###################
library(RcmdrMisc)
summary(cheese)
numSummary(cheese, statistics = c("mean", "sd", "se(mean)", "IQR", "quantiles", "cv"), quantiles=c(0,.25,.5,.75,1))

###################
####成對散布圖#####
###################
cheese <- as.data.frame(cheese)

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, ...)
}


pairs(cheese, 
      panel = panel.smooth,
      diag.panel = panel.hist, cex.labels = 2, font.labels = 2,
      labels = c("taste", "H2S", "Lactic"),
      family = "NotoSan")

ggscatmat(cheese, columns = 1:3)

##################
###相關係數矩陣###
##################
library(Hmisc)

corre <- rcorr(as.matrix(cheese))

# 寫一個FUN
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame( row = rownames(cormat)[row(cormat)[ut]], 
              column = rownames(cormat)[col(cormat)[ut]], cor =(cormat)[ut], p = pmat[ut] )
}

# 合併相關係數跟P值為一個矩陣
corre_ALL <- flattenCorrMatrix(corre$r, corre$P)

# 加上*號
corre_ALL[corre_ALL[, 4] <0.05, 5] <- "*"
corre_ALL[corre_ALL[, 4] <0.01, 5] <- "**"
corre_ALL[corre_ALL[, 4] <0.001, 5] <- "***"

library(corrplot)
corrplot(cor(cheese), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

###############
###複回歸模型##
###############
model_M <- lm(taste ~ H2S + Lactic, data = cheese)
summary(model_M)

-27.592 + 3.946*x + 19.887* y
###############
### 信賴區間 ##
###############
H2S_cof <- confint(model_M, "H2S", level = 0.95)
Lactic_cof <- confint(model_M, "Lactic", level = 0.95)

##################
###聯合信賴區間###
##################
# install.packages("ellipse")
library(ellipse)
attach(cheese)

plot(ellipse(model_M, which = c('H2S', 'Lactic'), level = 0.95), type = 'l',
     main = "Confidence Region and Confidence Interval")
points(model_M$coefficients['H2S'], model_M$coefficients['Lactic'], pch = 16) 
abline(v = H2S_cof[1], lty = 2) ; abline(v = H2S_cof[2], lty = 2)
abline(h = Lactic_cof[1], lty = 2) ; abline(h = Lactic_cof[2], lty = 2)

detach(IQ)

#############
### 配適值 ##
#############
pre <- data.frame(H2S = 7, Lactic = 1.5)
A <- predict(model_M, pre, se = T)
A

# 預估值se要+sigam^2
pre_se <- sqrt((A$se.fit)^2 + 9.942^2)

# 寫函數整理
cheese_fit <- function(x, y){
  pre <- data.frame(H2S = x, Lactic = y)
  A <- predict(model_M, pre, se = T)
  df <- A[["df"]]
  width <- qt(0.025, df)*A$se.fit
  FCI <- c(A$fit - width, A$fit + width)
  pre <- data.frame(H2S = x, Lactic = y, fit = A$fit, Upper = FCI[1], Lower = FCI[2])
  pre
}

#############
### 預估值 ##
#############
cheese_pred <- function(x, y){
  pre <- data.frame(H2S = x, Lactic = y)
  A <- predict(model_M, pre, se = T)
  df <- A[["df"]]
  width <- qt(0.025, df)*pre_se
  FCI <- c(A$fit - width, A$fit + width)
  pre <- data.frame(H2S = x, Lactic = y, pred = A$fit, Upper = FCI[1], Lower = FCI[2])
  pre
}

cheese_fit(7, 1.5)
cheese_pred(7, 1.5)


