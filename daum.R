setwd("D:/R-workspace/Rdata")
install.packages("rvest")
install.packages("stringr")
library(rvest)
library(stringr)

title=c()
url=c()
press=c()
body=c()
time=c()

url_b="https://news.daum.net/breakingnews/?page=1"
t_css="#mArticle ,tit_thumn ,desc_thumb"
pt_css=".info_news"
b_css=".desc_thumb .link_txt"

cr_url=paste0(url_b, 1, sep="")
hdoc=read_html(cr_url)
t_node=html_nodes(hdoc, t_css)
pt_node=html_nodes(hdoc, pt_css)
b_node = html_nodes(hdoc, b_css)

url_part=html_attr(t_node, "href")
title_part=html_text(t_node)
pt_part=html_text(pt_node)
time_part=str_sub(pt_part, -5)
time_part
press_part=str_sub(pt_part, end=-9)
press_part

b_part=html_text(b_node)
b_part=str_trim(b_part, side="both")

title=c(title, title_part)
press=c(press, press_part)
time=c(time, time_part)
body=c(body, b_part)
url=c(url, url_part)
}
title
press
time
body
url
news=data.frame(title,press,time,body, url)
write.csv(news, "news0706.csv")
View(news)
