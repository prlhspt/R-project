male <- c(10, 40, 60, 20)
female <- c(21, 60, 70, 30)
score <- cbind(male, female)
score
score[2,]
score[, "female"]
score[3, 2]

id <- c(10, 20, 30, 40, 50)

name <- c("John", "Tom", "Paul", "Jane", "Grace")

score <- c(95, 46, 98, 74, 85)

df <- data.frame(id, name, score)

df

df$score
df[,3]
df[,'score']
df[,c("id", "score")]
df[2,3]

str(iris)
unique(iris[,5])
table(iris[,"Species"])

class(iris)
class(state.x77)
is.matrix(iris)
is.data.frame(iris)
# data frame to matrix

iris.m <- as.matrix(iris[,1:4])

head(iris.m)
class(iris.m)

​

# matrix to data frame

st <- data.frame(state.x77)

head(st)
class(st)

# data frame to matrix

tmp <- iris[,-5]

class(tmp)

tmp2 <- as.matrix(tmp)

class(tmp2)

dim(st)
str(st)
st["Florida",]
st["Ohio", c("Population", "Income")]
st["Income"]>4500
condi <- "Area">100000&"Frost">120
st[condi]
a <- st["Illiteracy">2.0, "Income"]
mean(a)
b <- st["Illiteracy"<2.0, "Income"]
b
str(st$Illiteracy)
st["Illiteracy"]
mean(a)
mean(b)
mean(a)-mean(b)

st["Illiteracy" > 2.0,"Income"]

a <- st["Illiteracy" > 2.0,"Income"]
a
mean(a)

b <- st["Illiteracy" < 5,"Income"]

b <- st["Illiteracy">0, "Income"]
b
mean(b)

iris
myiris <- list(Group=iris[,"Species"], Data=iris[, 1:4])
myiris
Weekdays <- factor(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
Weekdays
as.numeric(Weekdays)
Weekdays[10] <- "WHY"

favorite.color <- c("red", "green", "yellow", "red", "green", "red", "red")

sum <- table(favorite.color) # 도수분포표

sum

barplot(sum, main="Favorite color") # 막대그래프

ans=c("Y","Y","N","Y","Y") 
table(ans) # 도수분포표 출력

table(ans)/length(ans) # 비율 출력

head(mtcars) # 자동차 모델별 제원

carb <- mtcars[,"carb"] # 기화기 수

table(carb) # 도수분포표

barplot(table(carb), main="Barplot of Carburetors", xlab="#of carburetors", ylab="frequency")

par(mfrow=c(1,3)) # 1x3 윈도우 생성

barplot(table(mtcars$carb), main="Barplot of Carburetors", xlab="#of carburetors", ylab="frequency", col="blue")

barplot(table(mtcars$cyl), main="Barplot of Cylender", xlab="#of cylender", ylab="frequency", col="red")

barplot(table(mtcars$gear), main="Barplot of Grar", xlab="#of gears", ylab="frequency", col="green")

favorite.color <- c("red", "green", "yellow", "red", "green", "red", "red") #벡터 생성

sum <- table(favorite.color ) # 도수분포표

pie(sum, main="Favorite color")


edu <- infert[, "education"]
edu
str(edu)
unique(edu)
table(edu)
sum <- table(edu)
barplot(sum, main="Education")
barplot(sum, main="Education", col="blue")

mydata = c(50,60,100,75,200)

mydata.big = c(mydata, 50000)

mean(mydata) # 평균

mean(mydata.big)

median(mydata) # 중앙값 median(mydata.big)

mean(mydata, trim=0.2) # 절사평균

mean(mydata.big, trim=0.2)

quantile(mydata) # 사분위수quantile(mydata, (0:10)/10)

summary(mydata)

fivenum(mydata) # quantile()과 비슷

diff(range(mydata)) # 최대값-최소값

var(mydata) # 분산

sd(mydata) # 표준편차

head(state.x77)

st.income <- state.x77[,"Income"]

boxplot(st.income, ylab="Income value")

iris

boxplot(Petal.Width~Species,data=iris, ylab="Petal.Width")

st.income <- state.x77[,"Income"]

hist(st.income, # data
     
     main="Histogram for Income", # 제목
     
     xlab ="income", # x축 레이블
     
     ylab="frequency", # y축 레이블
     
     border="blue", # 막대 테두리색
     
     col="green", # 막대 색
     
     las=2, # x축 글씨방향(0~3)
     
     breaks=5) # x축 막대 개수 조절

score <- c(40,55,90,75,59,60,63,65,69,71)

stem(score, scale=2)

wt <-mtcars$wt

mpg <- mtcars$mpg

plot(wt, mpg, # 2개 변수(x축,y축)
     
     main="Car Weight-mpg", # 제목
     
     xlab="Car Weight ", # x축 레이블
     
     ylab="Miles Per Gallon ", # y축 레이블 
     
     col="red", # point 의 color
     
     pch=19) # point 의 종류

drat

vars <- c("mpg","disp","drat","wt") # 대상 변수

target <- mtcars[,vars]

pairs(target, main="Multi plots")
# 대상 데이터 

iris.2 <- iris[,3:4] # 데이터

point <- as.numeric(iris$Species) # 포인트 모양

color <- c("red","green","blue") # 포인트 컬러

plot(iris.2, main="Iris plot", pch=c(point), col=color[point])

# -----------------

cars

speed <- cars$speed

dist <-  cars$dist

plot(speed, dist,
     
     main = "speed 와 dist",
     
     xlab = "speed",
     
     ylab = "dist",
     
     col = "pink",
     
     pch = 11)

pressure

temperature <- pressure$temperature

pressure <- pressure$pressure

plot(temperature, pressure,
     
     main = "temperature와 pressure의 상관관계",
     
     xlab = "temperature",
     
     ylab = "pressure",
     
     col = "orchid2",
     
     pch = 13)

state.x77

vars <- c("Population", "Income", "Illiteracy", "Area")

target <- state.x77[,vars]
target <- state.x77
class(target)
target
pairs(target, main = "변수간 산점도 상관관계")

iris

iris.2 <- iris[,1:2]

point <- as.numeric(iris$Species)

color <- c("indianred2", "lightpink", "royalblue2")

plot(iris.2,
     
     main = "꽃받침의 길이와 폭의 분포",
     
     pch = c(point),
     
     col = color[point])


beers = c(5,2,9,8,3,7,3,5,3,5)

bal = c(0.1,0.03,0.19,0.12,0.04,0.0095,0.07, 0.06,0.02,0.05)

tbl = data.frame(cbind(beers,bal))

tbl;
class(tbl)

plot(bal~beers,data=tbl) # 산점도

res=lm(bal~beers,data=tbl) # 회귀식 도출
res=lm(bal~beers)
abline(res) # 회귀선그리기

cor(beers,bal) # 상관성 분석 시행

month = 1:12

late = c(5,8,7,9,4,6,12,13,8,6,6,4)

plot(month, # x data
     
     late, # y data
     
     main="Late students",
     
     type= "l", # 그래프의 종류 선택(알파벳)
     
     lty=1, # 선의 종류(line type) 선택
     
     lwd=1, # 선의 굵기 선택
     
     xlab="Month ", # x축 레이블
     
     ylab="Late cnt" # y축 레이블
)

month = 1:12

late1 = c(5,8,7,9,4,6,12,13,8,6,6,4)

late2 = c(4,6,5,8,7,8,10,11,6,5,7,3)

plot(month, # x data
     
     late1, # y data
     
     main="Late students",
     
     type= "b", # 그래프의 종류 선택(알파벳).
     
     lty=1, # 선의 종류(line type) 선택
     
     col="red", # 선의 색깔 선택
     
     xlab="Month ", # x축 레이블
     
     ylab="Late cnt" # y축 레이블 
     )
    
     lines(month, late2, type = "b", col = "blue")
     
     years = 2015:2026
     
     Population = c(51014, 51245, 51446, 51635, 51811, 51973, 52123, 52261,
                    
                    52388, 52504, 52609, 52704)
     
     plot(years, Population,
          
          main="2015년부터 2026년까지 예상 인구수",
          
          type="l",
          
          lty=5,
          
          col="deeppink2",
          
          xlab = "years",
          
          ylab = "Population")
     
     years2 = c(20144, 20151, 20152, 20153, 20154, 20161, 20162, 20163,
                
                20164, 20171, 20172, 20173)
     
     man = c(73.9, 73.1, 74.4, 74.2, 73.5, 73.0, 74.2, 74.5, 73.8,
             
             73.1, 74.5, 74.2)
     
     woman = c(51.4, 50.5, 52.4, 52.4, 51.9, 50.9, 52.6, 52.7, 52.2,
               
               51.5, 53.2, 53.1)
     
     plot(years2, man,
          
          main = "남녀의 경제활동참가율 통계",
          
          type = "b",
          
          lty = 1,
          
          ylim = c(min(woman), max(man)),
          
          col = "red3",
          
          xlab = "years",
          
          ylab = "man")
     
     lines(years2, woman,
           
           type = "b",
           
           col = "blue3")
     
     point <- as.numeric(iris$Species) # 포인트 모양
     
     color <- c("red","green","blue") # 포인트 컬러
     
     pairs(iris[,-5], pch=c(point), col=color[iris[,5]] )
    
