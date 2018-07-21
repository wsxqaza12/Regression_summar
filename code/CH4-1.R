setwd("C:/Users/User/Documents/Regression_summar/")
library(data.table)

bodyfat0 <- fread("bodyfat.csv")
bodyfat <- bodyfat0[, 2:11]
###################
####�y�z�ʲέp#####
###################
library(RcmdrMisc)
summary(bodyfat)
numSummary(bodyfat, statistics = c("mean", "sd", "se(mean)", "IQR", "quantiles", "cv"), quantiles=c(0,.25,.5,.75,1))

###################
####���ﴲ����#####
###################
library(showtext)
library(ggplot2)
library(GGally)
font_add("NotoSan", "NotoSansMonoCJKtc-Regular.otf")

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, ...)
}

col <- names(bodyfat)

pairs(bodyfat,
      panel = panel.smooth,
      diag.panel = panel.hist,
      cex.labels = 1.2,
      font.labels = 1.5,
      labels = col)

# ggscatmat(bodyfat, columns = 1:11, alpha = I(1/10))+
#   labs(title = "���ﴲ����")

ggpairs(bodyfat,
        title = "���ﴲ����",
        lower = list(continuous = wrap("points", color = "red", alpha = I(1/10)), 
                     combo = wrap("box", color = "orange", alpha = 0.3), 
                     discrete = wrap("facetbar", color = "yellow", alpha = 0.3) ), 
        diag = list(continuous = wrap("densityDiag",  color = "blue", alpha = 0.5) ))+
  theme(text=element_text(size=12,  family="BL"))
  
# pdf("try.pdf")
# dev.off()

##################
###�����Y�Ưx�}###
##################
library(Hmisc)

corre <- rcorr(as.matrix(bodyfat))

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
colnames(corre_ALL)[5] <- "Significant" 

library(corrplot)
corrplot(cor(bodyfat), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

################
###�Ʀ^�k�ҫ�###
################
M0 <- lm(fat ~ 1, data = bodyfat)
MA <- lm(fat ~ ht + wt + hip + fore, data = bodyfat)

summary(M0)
summary(MA)

options(contrasts = c("contr.sum", "contr.poly"))

# Type I tests
anova(MA)

library(car)
# Type II tests
Anova(MA, type="II")
# Type III tests
Anova(MA, type="III") 

# �ϹL�ө�
MA_t <- lm(fat ~ fore + hip + wt + ht, data = bodyfat)

# Type I tests
anova(MA_t)
# Type II tests
Anova(MA_t, type="II")
# Type III tests
Anova(MA_t, type="III") 

# Nested model comparison
MB <- lm(fat ~ ht + wt, data = bodyfat)
anova(MA, MB, test = "F")