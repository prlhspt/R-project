setwd("C:/Users/LimHyunSeok/Documents/workspace/R-project/Rdata")
View(attitude)
cov(attitude)
cor(attitude)
with(attitude, cor.test(attitude$rating, attitude$complatins))
cor= test(attitude$rating, attitudes$complatins)


data=read.csv("cars.csv")
data
out=lm(dist~speed, data=data)
summary(out)


data=read.csv("salary.csv")
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
# ?€λ³? ?°?΅ λ¬Έμ 
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
position=bb2013$?¬μ§?
head(position)

base2_pos = bb2013[, c(2, 4:11)]
base2_pos2 = aggregate(base2_pos[, 2:9], by = list(?¬μ§?=base2_pos$?¬μ§?), sum)
head(base2_pos2)
rownames(base2_pos2) = base2_pos2[, 1]
head(base2_pos2)
library(lattice)
parallel(~bb2013[,4:11]|position.horizontal, axis=F, col1)
parallel(base2_pos2[,  2:9], horizontal.axis=F, col=1)

# ??λ³? ?? μ’ν
team = bb2013$??
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
