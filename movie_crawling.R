setwd("D:/R-Workspace/Rdata")
library(dplyr)
install.packages("ggmap")
library(ggmap)
library(stringr)
library(rvest)

register_google(key="AIzaSyCcHwEAt_eIkxdA_qNS-rPwUc8WjBzlZN8")
tt=get_map(location = "청주",
        zoom = 15,
        maptype = 'satellite',# hybrid.satellite, terrian,...
        source='google')
ggmap(tt)

plot.new()
frame()
geocodeQueryCheck()
geocode(location = '청주',
        output='latlon',
        source='google')

geocode(location = enc2utf8(x= "청주$language=ko"),
        output='latlon',
        source='google')

myloc=geocode(location = enc2utf8(x= "청주$language=ko"),
              output='latlon',
              source='google')
myloc
center=c(myloc$lon.myloc$lat)
qmap(location = center,
     zoom=18
     maptype='hybrid',
     source='google')+
  geom_point(data=myloc,
             mapping=aes(x=lon, y=lat),
             sharp='*',
             color='red',
             stroke=18, size=10)

     
url = "https://namu.wiki/w/%EC%84%9C%EC%9A%B8%ED%8A%B9%EB%B3%84%EC%8B%9C%EC%9D%98%20%EB%8C%80%ED%95%99%EA%B5%90%20%EB%AA%A9%EB%A1%9D"

hdoc=read_html(trl, encoding='UTF-8')
df=hdoc %>% 
  html_nodes(", wiki-paragraph a") %>% 
  html_text()
head(df, 50)
#str_detect(df, pattern='대학교')
univ=ifelse(str_detect((df, pattern='대학교')))
univ
kk=univ %>% 
  data.frame()
kk=Filter(function(x){ncahr(x)>=5}, univ)
kk
univName=kk[2:28]
univName
univCord=geocode

library(rvest)
library(stringr)
library(dplyr)

title=c()
grade=c()
time=c()

t_css=".color_b"
gr_css=".list_netizen_score em"
pt_css=".title+ .num"

base_url="https://movie.naver.com/movie/point/af/list.nhn?&page="

for(i in 1:10){
cr_url=paste0(base_url, i)
hdoc=read_html(cr_url, encoding="CP949")
n_title=html_nodes(hdoc, t_css)
n_gr=html_nodes(hdoc, gr_css)
n_pt=html_nodes(hdoc, pt_css)

title_part=html_text(n_title)
grade_part=html_text(n_gr)
grade_part
pt_part=html_text(n_pt)
time_part=str_sub(pt_part, -8)


title=c(title, title_part)
grade=c(grade, grade_part)
time=c(time, time_part)
}

movie=data.frame(title, grade, time)
View(movie)

write.csv(movie, "movie.csv")
data=read.csv("movie.csv")
head(data)
top10_A = data %>% 
  select(title, grade) %>% 
  group_by(title) %>% 
  summarise(total=sum(grade),
            count=n()) %>% 
  arrange(desc(total), desc(count)) %>% 
  head(10)
top10_A
ggplot(data=top10_A,aes(x=title, y=total))+
  geom_col(fill='red')+
  geom_text(aes(label=top10_A$total), vjust=-0.2, col='blue')
