df <- data.frame(sex = c("M", "F", NA, "M", "F"),
                 score = c(5, 4, 3, 4, NA))
df

is.na(df)

table(is.na(df$score))
table(is.na(df))

mean(df$score)

library(dplyr)
kk = df%>%
       filter(!is.na(score))

mean(kk$score)
df
df1=df$score.na.rm=T
df1

na.omit(df)
df
mean(df$score,na.rm = T)
sum(df$score,na.rm = T)

setwd("D:/R-workspace/Rdata")
exam=read.csv("csv_exam.csv")
exam
exam[c(3, 8, 15), "math"]=NA
exam

exam %>% 
  summarise(mean_math=round(mean(math, na.rm=T)))
kk=table(is.na(exam$math))
tt=barplot(kk, col=rainbow(2), ylim=c(0,20))
text(tt, kk, label=paste0(kk, "건"), pos=3)

mean(exam$math, na.rm=T)
exam$math=ifelse(is.na(exam$math), 55, exam$math)
table(is.na(exam$math))
exam

library(ggplot2)
mpg=as.data.frame(ggplot2::mpg)
mpg
mpg[c(65, 124, 131, 153, 212), "hwy"]=NA
table(is.na(mpg$hwy))

df_mpg=mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy=round(mean(hwy, na.rm=T),1))
df_mpg
ggplot(data=df_mpg, aes(x=drv, y=mean_hwy)) # infra
ggplot(data=df_mpg, aes(x=drv, y=mean_hwy)) + geom_col()
ggplot(data =mpg, aes(x=drv))+geom_bar()


outlier <- data.frame(sex = c(1, 2, 1, 3, 2, 1),
                      score = c(5, 4, 3, 4, 2, 6))
outlier
outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex)
outlier

outlier$score <- ifelse(outlier$score > 5, NA, outlier$score)
outlier
outlier %>% 
  group_by(sex, na.rm=T) %>% 
  summarise(mean(score, na.rm=T))

outlier %>% 
  filter (!is.na(sex) & !is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(mean_score=mean(score))

as.data.frame(outlier %>% 
  filter (!is.na(sex) & !is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(mean_score=mean(score)))

mpg=as.data.frame(ggplot2::mpg)
boxplot(mpg$hwy, horizontal = T, col=2)
summary(mpg$hwy)
IQR(mpg$hwy)
hist(mpg$hwy, probability =T)
lines(density(mpg$hwy), col=2)
lines(density(mpg$hwy), type='h', col=2)
boxplot(mpg$hwy)$stats
mpg$hwy=ifelse(mpg$hwy<12 | mpg$hwy>37, NA, mpg$hwy)
kk = table(is.na(mpg$hwy))
tt=barplot(kk, col=rainbow(2), ylim=c(0, 250))
text(tt, kk, label = paste0(kk, "건"), col=2, cex=2, pos = 3)



mpg <- as.data.frame(ggplot2::mpg)           
mpg[c(65, 124, 131, 153, 212), "hwy"] <- NA
mpg
library(dplyr)
library(ggplot2)
# Q1.
table(is.na(mpg))
table(is.na(mpg$drv))
table(is.na(mpg$hwy))

# Q2.
mpg %>% 
  group_by(drv) %>% 
  filter(!is.na(hwy)) %>% 
  summarise(mean(hwy, ns.na=T))


# Q1. 
mpg <- as.data.frame(ggplot2::mpg)                  # mpg 데이터 불러오기
mpg[c(10, 14, 58, 93), "drv"] <- "k"                # drv 이상치 할당
mpg[c(29, 43, 129, 203), "cty"] <- c(3, 4, 39, 42)  # cty 이상치 할당

table(is.na(mpg))

mpg$drv <- ifelse(mpg$drv %in% c(4, "f", "r"), mpg$drv, NA)

# Q2.
boxplot(mpg$cty)$stats

mpg$cty <- ifelse(mpg$cty < 10 | mpg$cty>25,NA, mpg$cty)

table(is.na(mpg$cty))

boxplot(mpg$cty)$stats 


# Q3.
df_mpg = mpg %>% 
  filter(!is.na(drv)& !is.na(cty)) %>% 
  group_by(drv) %>% 
  summarise(mean_cty = mean(cty))

df_mpg

install.packages("plotly")
library(plotly)
library(ggplot2)
tt = ggplot(data=df_mpg, aes(x=reorder(drv, -mean_cty), y=mean_cty))+
  geom_col(fill=rainbow(3))+coord_flip()
ggplotly(tt)
df_mpg
ggplot(data=df_mpg, aes(x=reorder(drv, mean_cty),  y=mean_cty))+geom_col(fill=rainbow(3))+coord_flip()+
  ylab("도시연비비")+xlab('구동시간')
ggplotly(tt)

ggplot(data=mpg, aes(x=displ, y=hwy)) + geom_point()
