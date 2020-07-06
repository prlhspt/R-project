#건강, 과즙, 사피로 테스트(P가 0.01 이상이면 (실물데이터는 0.01, 그냥은 0.025) 가공할 가치가 있다.)
library(dplyr)
setwd("D:/R-workspace/Rdata")
data <- read.csv("sales_data.csv")

View(data)



data_new = data %>%
  filter(CATEGORY %in%  c("건강음료", "과즙음료", "차음료"))

shapiro.test(data_new)

View(data_new)
write.csv(data_new, "sales_data_new.csv")

shapiro.test( data_new$YM )
shapiro.test( data_new$PRICE )
shapiro.test( data_new$SALEDAY )
