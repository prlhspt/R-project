setwd("D:/workspace/r/Rdata")
data=read.csv("sales_data_new.csv")
class(data)
he <- data[1:60,]
fr <- data[61:120,]
te <- data[121:180,]

#이상치 제거
he <- he[-c(22,54,39,45,48,37),]
te <- te[-c(42,45,52),]

View(he)

shapiro.test(health$PRICE)
shapiro.test(health$YM)
shapiro.test(health$ITEM_CNT)
shapiro.test(health$QTY)
shapiro.test(health$MAXTEMP)
shapiro.test(health$SALEDAY)
shapiro.test(health$RAIN_DAY)
shapiro.test(health$HOLIDAY)

shapiro.test(fruit$PRICE)
shapiro.test(fruit$YM)
shapiro.test(fruit$ITEM_CNT) # 0.37
shapiro.test(fruit$QTY) # 0.255
shapiro.test(fruit$MAXTEMP)
shapiro.test(fruit$SALEDAY)
shapiro.test(fruit$RAIN_DAY)
shapiro.test(fruit$HOLIDAY)

shapiro.test(tea$PRICE)
shapiro.test(tea$YM)
shapiro.test(tea$ITEM_CNT)
shapiro.test(tea$QTY) # 0.08
shapiro.test(tea$MAXTEMP)
shapiro.test(tea$SALEDAY)
shapiro.test(tea$RAIN_DAY)
shapiro.test(tea$HOLIDAY)

vars <- c("QTY", "ITEM_CNT", "PRICE", "MAXTEMP", "SALEDAY","RAIN_DAY","HOLIDAY")
vars2 <- c("ITEM_CNT", "PRICE", "MAXTEMP", "SALEDAY","RAIN_DAY","HOLIDAY")
he <- he[,vars]
he
health$PRICE

par(mfrow=c(2,4))

plot(health$QTY~health$YM)
plot(health$QTY~health$ITEM_CNT)
plot(health$QTY~health$PRICE)
plot(health$QTY~health$MAXTEMP)
plot(health$QTY~health$SALEDAY)
plot(health$QTY~health$RAIN_DAY)
plot(health$QTY~health$HOLIDAY)
install.packages("corrplot")
library(corrplot)

health_cor <- cor(he[, vars])
fruit_cor <- cor(fr[, vars])
tea_cor <- cor(te[, vars])

pairs(health_cor, panel=panel.smooth)
corrplot(health_cor, method="number")

pairs(fruit_cor, panel=panel.smooth)
corrplot(fruit_cor, method="number")

pairs(tea_cor, panel=panel.smooth)
corrplot(tea_cor, method="number")

idx <- sample(1:nrow(he), size=nrow(he)*0.7, replace=F)
he_train <- he[idx,]
he_test <- he[-idx,]
he_train

#he_train <- he[1:45,]
#he_test <- he[45,60,]
#he_train

library(nnet)
nnet.fit <- nnet(QTY~.,data=he_train, size=5)
summary(nnet.fit)
nnet.yhat <- predict(nnet.fit, newdata=he_test, type="raw")
mean((nnet.yhat-he_test$QTY)^2)

lm.fit2 <- step(lm.fit, method="both")
summary(lm.fit2)
lm.yhat2 <- predict(lm.fit2, newdata=he_test)
sqrt(mean((lm.yhat2-he_test$QTY)^2))



library(randomForest)
set.seed(1)
rf.fit <- randomForest(QTY~., data=he_train, mtry=6, importance=T)
rf.fit
importance(rf.fit)
rf.yhat <- predict(rf.fit, newdata=he_test)
mean((rf.yhat-he_test$QTY)^2)

states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
class(states)
leaps <-regsubsets(Murder ~ ., data=states, nbest=4)
plot(leaps, scale="adjr2")


cor(he$QTY, he$SALEDAY)
cor(he$QTY, he$PRICE)

lm(QTY~vars2, data=he)
vars2
m1 <- lm(he$QTY~he$SALEDAY)
plot(he$QTY~he$SALEDAY)
abline(m1, col='blue')

m1 <- lm(he$QTY~he$SALEDAY)
summary(m1)



plot(fr$QTY~fr$MAXTEMP)
abline(m2, col='blue')

m2 <- lm(fr$QTY~fr$MAXTEMP)
summary(m2)

m3_1 <- lm(te$QTY~te$MAXTEMP)
summary(m3_1)

plot(te$QTY~te$.)
abline(m2, col='blue')

m3_2 <- lm(te$QTY~te$SALEDAY)
summary(m3_2)
he[,vars]



# ---------------------------------

he <- data[1:60,]
fr <- data[61:120,]
te <- data[121:180,]

#이상치 제거
he <- he[-c(22,54,39,45,48,37),]
te <- te[-c(41,44,51),]

vars <- c("QTY", "ITEM_CNT", "PRICE", "MAXTEMP", "SALEDAY","RAIN_DAY","HOLIDAY")
m1 = lm(QTY~., data=he[,vars])
summary(m1)

m1.fit <- step(m1,method="both")
summary(m1.fit)
plot(QTY~.,data=he)
he
he
he_cor <- cor(he[, vars])
he_cor
m1_cor <- cor(he[, vars])
corrplot(health_cor, method="number")

vars <- c("QTY", "ITEM_CNT", "PRICE", "MAXTEMP", "SALEDAY","RAIN_DAY","HOLIDAY")
m2 = lm(QTY~., data=fr[,vars])
summary(m2)

vars <- c("QTY", "ITEM_CNT", "MAXTEMP", "SALEDAY","RAIN_DAY","HOLIDAY")
m3 = lm(QTY~., data=te[,vars])
summary(m3)


fruit_cor <- cor(fr[, vars])
tea_cor <- cor(te[, vars])

pairs(fruit_cor, panel=panel.smooth)
corrplot(fruit_cor, method="number")

pairs(tea_cor, panel=panel.smooth)
corrplot(tea_cor, method="number")