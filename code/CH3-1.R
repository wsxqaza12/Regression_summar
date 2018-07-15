setwd("~/Reg")
library(data.table)
library(dplyr)
library(showtext)
library(GGally)

sbp <- fread("sbp100.csv")

###################
####�y�z�ʲέp#####
###################
# install.packages("RcmdrMisc")
library(RcmdrMisc)
summary(sbp)
numSummary(sbp, statistics=c("mean", "sd", "se(mean)", "IQR", "quantiles", "cv"), quantiles=c(0,.25,.5,.75,1))

###################
####���ﴲ����#####
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
      labels = c("�αi��", "�~��", "��M��T�J��"),
      family = "kaiu")

ggpairs(sbpy[, 1:3])

##################
###�����Y�Ưx�}###
##################
library(Hmisc)

cor(sbpy[, 1:3])
corre <- rcorr(as.matrix(sbpy[, 1:3]))

# �g�@��FUN
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame( row = rownames(cormat)[row(cormat)[ut]], 
                                      column = rownames(cormat)[col(cormat)[ut]], cor =(cormat)[ut], p = pmat[ut] )
}

# �X�֬����Y�Ƹ�P�Ȭ��@�ӯx�}
corre_ALL <- flattenCorrMatrix(corre$r, corre$P)

# �[�W*��
corre_ALL[corre_ALL[, 4] <0.05, 5] <- "*"
corre_ALL[corre_ALL[, 4] <0.01, 5] <- "**"
corre_ALL[corre_ALL[, 4] <0.001, 5] <- "***"

library(corrplot)
corrplot(cor(sbpy[, 1:3]), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

pdf(file = "myplot.pdf", bg = "transparent")
plot(x,y,type='l')
dev.off()

##############
###²��^�k###
##############
model1 <- lm(sbp ~ age, data = sbpy)
summary(model1)

# ��H��϶� �ۥѫ�98��T����
# �k1�A�ϥ�T�˩w�h�h�[��
qt(0.975, 98)
B0_cofin <- c(96.68322 - qt(0.975, 98)*4.03455, 96.68322 + qt(0.975, 98)*4.03455)
B1_cofin <- c(0.60657 - qt(0.975, 98)*0.07916, 0.60657 + qt(0.975, 98)*0.07916)
# �k2�Aconfint��ƪ����p��
confint(model1, "age", level = 0.95)

# ø�� 
fig <- ggplot(sbpy, aes(x = age, y = sbp))
fig + geom_point()+
  geom_smooth(method = "lm") 

# # # # # # # #
# # # # # # # #
model2 <- lm(sbp ~ chol, data = sbpy)
summary(model2)

# ��95%�H��϶� �ۥѫ�98��T����
# �k1�A�ϥ�T�˩w�h�h�[��
qt(0.975, 98)
B0_cofin <- c(105.80713 - qt(0.975, 98)*7.68916, 105.80713 + qt(0.975, 98)*7.68916)
B1_cofin <- c(0.09231 - qt(0.975, 98)*0.03499, 0.09231 + qt(0.975, 98)*0.03499)
# �k2�Aconfint��ƪ����p��
confint(model2, "chol", level = 0.95)

# ø�� �Υt�@�ؤ�k
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
###����ܼ�##
#############
# install.packages("car")
# �ϥ�car�M��̪�avPlot�A�e�XAdded-Variable Plot��
library(car)

model_M <- lm(sbp ~ age + chol, data = sbpy)
age <- sbpy$age
chol <- sbpy$chol

par(mfcol = c(1,2))
crPlot(model_M, "age"); crPlot(model_M, "chol")
avPlot(model_M, "age"); avPlot(model_M, "chol")

###############
###�Ʀ^�k�ҫ�##
###############
model_M <- lm(sbp ~ age + chol, data = sbpy)
summary(model_M)
