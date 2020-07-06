install.packages("multilinguer")
library(multilinguer)
install_jdk()
install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), 
                 type = "binary")
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", 
                        INSTALL_opts=c("--no-multiarch"))
library(KoNLP) #최종적으로 "KoNLP" 패키지를 불러옵니다

setwd("C:/Users/LimHyunSeok/Documents/workspace/R-project/Rdata")
library(rvest)
library(stringr)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(wordcloud2)

cnt=c()
b_url="https://www.bskorea.or.kr/bible/korbibReadpage.php?version=GAE&book=sng&chap="
for(i in 1:8){
  cr_url=paste0(b_url,i)
  t_css="#tdBible1 span"
  hdoc=read_html(cr_url,encoding = "UTF-8")
  n_css=html_nodes(hdoc,t_css)
  cnt_part=html_text(n_css)
  cnt_part=gsub("\\d+","",cnt_part)
  cnt_part
  cnt_part=str_trim(cnt_part,side="both")
  cnt_part
  cnt=c(cnt,cnt_part)
}

txt=sapply(cnt, extractNoun, USE.NAMES = F)
txt=unlist(txt)
count=Filter(function(x){nchar(x)>=2},txt)
word=table(count)

kk=head(sort(word,decreasing = T),20)
kk
tt=barplot(kk,col=rainbow(20),ylim=c(0,300),las=2)
text(tt,kk,label=paste0(kk,"개"),pos=3,col=2)

library(RColorBrewer)
display.brewer.all()
palate=brewer.pal(9,"Greens")
wordcloud(names(word),
          freq=word,
          min.freq = 2,
          scale=c(5,0.5),
          random.order = F,
          random.color = T,
          colors=palate)
?wordcloud2()
wordcloud2(data=word,
           size=0.4,
           shape='diamond')

