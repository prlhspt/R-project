setwd("D:/R-Workspace/Rdata")
View(attitude)
cov(attitude)
cor(attitude)
with(attitude, cor.test(attitude$rating, attitude$complatins))
cor= test(attitude$rating, attitudes$complatins)


data=read.csv("cars.csv")
data
out=lm(dist~speed, data=data)
summary(out)


data=read.csv("programming.csv")
head(data)
out=lm(salary~experience+score, data=data)
