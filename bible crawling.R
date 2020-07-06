install.packages("wordcloud2")
setwd("D:/R-workspace/RData")
library(rvest)
library(stringr)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(wordcloud2)

cnt=c()
b_url="https://www.bskorea.or.kr/bible/korbibReadpage.php?version=GAE&book=gen&chap=1&sec=1"
cr_url=paste0(b_url, 1)
t_css="#td8ible1 span"
hdoc=read_html(cr_url, encoding="UTF-8")
n_css=html_nodes(hdoc, t_css)
cnt_part=html_text(n_css)
cnt_part=gsub("\\d+", "", cnt_part)
cnt_part
cnt=c(cnt, cnt_part)
}
library(KoNLP)
txt=sapply(cnt, extractNoun, USE.NAMES = F)

