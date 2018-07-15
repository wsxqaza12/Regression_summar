setwd("~/Reg")
library(data.table)
library(dplyr)


IQ <- fread("IQBirthOrderAge.csv")
modelT <- lm(iq ~ birth + age, data = IQ)
summary(modelT)

IQ_pre <- function(x, y){
  pre <- data.frame(birth = x, age = y)
  A <- predict(modelT, pre, se = T)
  df <- A[["df"]]
  width <- qt(0.025, df)*A$se.fit
  FCI <- c(A$fit - width, A$fit + width)
  pre <- data.frame(H2S = x, Lactic = y, fit = A$fit, Upper = FCI[1], Lower = FCI[2])
  pre
}
IQ_pre(1, 20)




pre <- data.frame(birth = 1, age = 20)
A <- predict(modelT, pre, se = T, level = 0.95)
A

sqrt(1.724^2 + 3.828^2)


fitted(modelT, pre)
predict(modelT)

confint(modelT)


K <- rbind(c(1, -1/2, -1/2), ## ctrl vs. average of trt1 and trt2
           c(1, -1, 0))      ## ctrl vs. trt1
fit.gh <- glht(fit, linfct = mcp(group = K))


Y <- as.matrix(IQ$iq)
X1 <- as.matrix(rep(1, 15))
X2 <- as.matrix(IQ$birth)
X3 <- as.matrix(IQ$age)

X <- cbind(X1, X2)
X <- cbind(X, X3)

B <- solve(t(X)%*%X)%*%t(X)%*%Y

(t(B)%*% t(X)%*% X%*% B) / 3 / 3.828^2

# 聯合信賴區間
library(ellipse)
BP.data<-read.table("BP.txt",header=T)
attach(IQ)
fit<-lm(iq ~ birth + age)

plot(ellipse(fit, which = c('birth', 'age'), level = 0.95), type = 'l')
points(fit$coefficients['birth'], fit$coefficients['age'], pch = 16) 
detach(IQ)
