setwd("D:/workspace/r/Rdata")
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

education=read.csv("http://datasets.flowingdata.com/education.csv")
head(education)

library(lattice)
parallel(education[2:7], horizontal.axis=FALSE, col=1)
summary(education$reading)
color=education$reading
parallel(education[,2:7], horizontal, axis=False, col=color+1)
summary(education$dropout_rate)
color=education$dropout_rate>5.3
color

data=read.csv("20140528_baseball.csv")
head(data)
model=prcomp(data[,2:6], scale=T)
model
summary(model)
plot(model)
mdeol=prcomp(data[,2:6], scale=T)
biplot

#############################

install.packages("aplpack")
library(aplpack)
# 다변량 연습 문제
data = read.csv("20140528_baseball.csv")
head(data)
rownames(data)=data[,1]
head(data)
stars(data[,2:6])
stars(data[,2:6], flip.labels = F, key.loc=c(9, 2))
stars(data[,2:6], flip.labels = F, key.loc=c(9, 3), draw.segments = )
faces(data[,2:6])

#2.
bb2013=read.csv("2013_baseball.csv")
head(bb2013)
position=bb2013$포지션
head(position)

base2_pos = bb2013[, c(2, 4:11)]
base2_pos2 = aggregate(base2_pos[, 2:9], by = list(포지션=base2_pos$포지션), sum)
head(base2_pos2)
rownames(base2_pos2) = base2_pos2[, 1]
head(base2_pos2)
library(lattice)
parallel(~bb2013[,4:11]|position.horizontal, axis=F, col1)
parallel(base2_pos2[,  2:9], horizontal.axis=F, col=1)

# 팀별 평행 좌표
team = bb2013$팀
head(team)
parallel(~bb2013[, 4:11]|team.horizontal, axis=F, col1)

#3.
rownames(bb2013)=bb2013[, 1]
rownames(bb2013)
head(bb2013)
model=prcomp(bb2013[,4:11], scale=T)
plot(model)
summary(model)
biplot(model)
