x=rnorm(100, 175, 2)
x
hist(x, breaks = 5, probability = T)
lines(density(x), col=2, type='h', lwd=0.5)
shapiro.test(x)

a=2
b=5

a+b

var1 <- c(1, 2, 5, 7, 8)
var1
var2=c(1:5)
var2

var3 = seq(1, 5)
var3

var4 = seq(1, 10, by=2)
var4

var1+2

var1+var2
str1="a"
str1

str5 = c("i", "am", "a", "boy")
str5
str5+2 # X

x=c(1, 4, 7)
x
mean(x)
max(x)
min(x)
sd(x)

paste(str5, collapse = ",")

install.packages("ggplot2")
library(ggplot2)
x <- c("a", "a", "b", "c")
x
qplot(x)
qplot(data=mpg, x=cty)
qplot(data=mpg, x = drv, y=hwy)
qplot(data=mpg, x=drv, y=hwy, geom="line")
qplot(data=mpg, x=drv, y=hwy, geom="boxplot")
qplot(data=mpg, x=drv, y=hwy, geom="boxplot", colour=drv)

num = c(80, 60, 70, 50, 90)
mean(num)
sd(num)


english <- c(90, 80, 60, 70)
math <- c(50, 60, 100, 20)
df_midterm=data.frame(english, math)
df_midterm
class <- c(1, 1, 2, 2)
df_midterm=data.frame(english, math,class)
mean(df_midterm$english)
sd(df_midterm$english)
sqrt(var(df_midterm$english))

sales <- data.frame(fruit = c("사과", "딸기", "수박"),
                       price = c(1800, 1500, 3000),
                       volume = c(24, 38, 13))
sales
mean(sales$price)
mean(sales$volume)
install.packages("readxl")
library(readxl)
setwd("d:/Rdata")
write.csv(df_midterm, "df_midterm.csv")
df_mid_test <- read.csv("df_midterm.csv")
df_mid_test

x=rnorm(100)
hist(x, probability = T)
shapiro.test(x)

plot.new()
hist(mpg$hwy, probability = T)
lines(density(mpg$hwy), col=2, lwd=1)
lines(density(mpg$hwy), col=2, type='h', lwd='1')
shapiro.test(mpg$hwy)

exam <- read.csv("csv_exam.csv")
exam

head(exam, 15)
tail(exam, 1)
View(exam)
dim(exam)
str(exam)
summary(exam)
boxplot(exam$math)
hist(exam$math)
boxplot(exam$math, horizontal = T)
boxplot(exam$math, horizontal = T, col=2 #color)

x=sample(0:100, 80, replace = T)
plot(x, pch=ifelse(x>=60, 10, 20), col=ifelse(x>=60, 2, 5))
abline(h=60, col=2, lwd=2)
mpg=as.data.frame(ggplot2::mpg)
head(mpg)
View(mpg)

install.packages("dplyr")
library(dplyr)
df_raw <- data.frame(var1 = c(1, 2, 1),
                      var2 = c(2, 3, 2))
df_raw
df_new=df_raw
df_new <- rename(df_new, v2 = var2)
df_new

mpg=as.data.frame(ggplot2::mpg)
mpg_new=mpg
mpg_new<-rename(mpg_new, city = cty, highway=hwy)
head(mpg_new)
write.csv(mpg_new, "mpg.csv")

df <- data.frame(var1 = c(4, 3, 8),
                 var2 = c(2, 6, 1))
df

df$var_sum <- df$var1 + df$var2  # var_sum 파생변수 생성
df
df$var_mean <- df$var_sum/2
df
mpg$total <- (mpg$cty + mpg$hwy)/2  # 통합 연비 변수 생성
head(mpg)

mpg$total <- (mpg$cty + mpg$hwy)/2  # 통합 연비 변수 생성
head(mpg$total)
boxplot(mpg$total, horizontal = T)
hist(mpg$total)
mpg$test=ifelse(mpg$total>=20, 'pass', 'fail')
qplot(mpg$test)
table(mpg$test)

mpg$grade <- ifelse(mpg$total >= 30, "A",
                    ifelse(mpg$total >= 20, "B", "C"))
qplot(mpg$grade) 
mpg$grade2 <- ifelse(mpg$total >= 30, "A",
                     ifelse(mpg$total >= 25, "B",
                            ifelse(mpg$total >= 20, "C", "D")))

qplot(mpg$grade2) 
midwest=as.data.frame(ggplot2::midwest)
head(midwest)
midwest = rename(midwest, total = poptotal)
midwest = rename(midwest, asian = popasian)
midwest$ratio <- midwest$asian/midwest$total*100
hist(midwest$ratio)
mean(midwest$ratio)
midwest$group <- ifelse(midwest$ratio > mean(midwest$ratio), "lerget", "small")
qplot(midwest$group)
