setwd("~/Reg")
library(data.table)
library(dplyr)
library(showtext)
library(GGally)

sbp <- fread("sbp100.csv")

###################
####描述性統計#####
###################
# install.packages("RcmdrMisc")
library(RcmdrMisc)
summary(sbp)
numSummary(sbp, statistics=c("mean", "sd", "se(mean)", "IQR", "quantiles", "cv"), quantiles=c(0,.25,.5,.75,1))

###################
####成對散布圖#####
###################
sbpy <- sbp %>% select(sbp, age, chol, smoke)
sbpy <- as.data.frame(sbpy)

# showtext.auto(enable = TRUE)
font_add("NotoSan", "NotoSansMonoCJKtc-Regular.otf")
# font_add("kaiu", "kaiu.ttf")

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, ...)
}


pairs(sbpy[, 1:3], col= as.integer(sbpy[, 4])+ 2,
      panel = panel.smooth,
      diag.panel = panel.hist, cex.labels = 2, font.labels = 2,
      labels = c("舒張壓", "年齡", "血清單固醇值"),
      family = "kaiu")

ggpairs(sbpy[, 1:3])

##################
###相關係數矩陣###
##################
library(Hmisc)

cor(sbpy[, 1:3])
corre <- rcorr(as.matrix(sbpy[, 1:3]))

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
corrplot(cor(sbpy[, 1:3]), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

pdf(file = "myplot.pdf", bg = "transparent")
plot(x,y,type='l')
dev.off()

##############
###簡單回歸###
##############
model1 <- lm(sbp ~ age, data = sbpy)
summary(model1)

# 算信賴區間 自由度98的T分布
# 法1，使用T檢定去去加減
qt(0.975, 98)
B0_cofin <- c(96.68322 - qt(0.975, 98)*4.03455, 96.68322 + qt(0.975, 98)*4.03455)
B1_cofin <- c(0.60657 - qt(0.975, 98)*0.07916, 0.60657 + qt(0.975, 98)*0.07916)
# 法2，confint函數直接計算
confint(model1, "age", level = 0.95)

# 繪圖 
fig <- ggplot(sbpy, aes(x = age, y = sbp))
fig + geom_point()+
  geom_smooth(method = "lm") 

# # # # # # # #
# # # # # # # #
model2 <- lm(sbp ~ chol, data = sbpy)
summary(model2)

# 算95%信賴區間 自由度98的T分布
# 法1，使用T檢定去去加減
qt(0.975, 98)
B0_cofin <- c(105.80713 - qt(0.975, 98)*7.68916, 105.80713 + qt(0.975, 98)*7.68916)
B1_cofin <- c(0.09231 - qt(0.975, 98)*0.03499, 0.09231 + qt(0.975, 98)*0.03499)
# 法2，confint函數直接計算
confint(model2, "chol", level = 0.95)

# 繪圖 用另一種方法
# install.packages("ggpubr")
library(ggpubr)
ggscatter(sbpy, x = "age", y = "sbp",
          add = "reg.line", conf.int = TRUE,    
          add.params = list(color = 11, fill = "lightgray"),
          ggtheme = theme_minimal()
)+
  stat_cor(method = "pearson", 
           label.x = 25, label.y = 150) 

#############
###選取變數##
#############
# install.packages("car")
# 使用car套件裡的avPlot，畫出Added-Variable Plot圖
library(car)

model_M <- lm(sbp ~ age + chol, data = sbpy)
age <- sbpy$age
chol <- sbpy$chol

par(mfcol = c(1,2))
crPlot(model_M, "age"); crPlot(model_M, "chol")
avPlot(model_M, "age"); avPlot(model_M, "chol")

###############
###複回歸模型##
###############
model_M <- lm(sbp ~ age + chol, data = sbpy)
summary(model_M)

