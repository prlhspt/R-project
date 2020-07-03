# filter
mpg=as.data.frame(ggplot2::mpg)
head(mpg)

# 1
mpg_di_lo = mpg %>% filter(displ<=4)
mpg_di_hi = mpg %>% filter(displ>=5)

mean(mpg_di_lo$hwy)
mean(mpg_di_hi$hwy)

# 2
mpg_au = mpg %>% filter(manufacturer=="audi")
mpg_to = mpg %>% filter(manufacturer=="toyota")

mean(mpg_au$cty)
mean(mpg_to$cty)

# 3
mpg_new = mpg %>%  filter(manufacturer %in% c("chevrolet", "ford",
                                               "honda"))
mean(mpg_new$hwy)



# select
mpg=as.data.frame(ggplot2::mpg)
head(mpg)

# 1
df = mpg %>% select(class, cty)
head(df)

# 2
df_suv = df %>% filter(class=="suv")
df_com = df %>% filter(class=="compact")
mean(df_suv$cty)
mean(df_com$cty)



# arrange
mpg=as.data.frame(ggplot2::mpg)
head(mpg)

# 1
mpg %>% filter(manufacturer == "audi") %>% 
  arrange(desc(hwy)) %>% 
  head(5)



# mutate
mpg=as.data.frame(ggplot2::mpg)
head(mpg)

# 1
mpg_new = mpg

mpg_new = mpg_new %>% 
  mutate(sum_hwy = (cty+hwy))

# 2
mpg_new = mpg_sum %>% 
  mutate(avg_hwy = (cty+hwy) / 2)

# 3
mpg_new %>% 
  arrange(desc(avg_hwy)) %>% 
  head(3)

# 4
mpg %>%
  mutate(sum_hwy=cty+hwy) %>% 
  mutate(avg_hwy=sum_hwy/2) %>% 
  arrange(desc(avg_hwy)) %>% 
  head(3)



# summarise, group_by
mpg=as.data.frame(ggplot2::mpg)
head(mpg)

# 1
mpg %>% 
  group_by(class) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  head

# 2
mpg %>% 
  group_by(class) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  arrange(desc(mean_cty))

# 3
mpg %>% 
  group_by(manufacturer) %>% 
  summarise(mean_hwy = mean(hwy)) %>% 
  arrange(desc(mean_hwy)) %>% 
  head(3)

# 4
mpg %>% 
  filter(class=="compact") %>% 
  group_by(manufacturer) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head



# left_join, bind_rows
mpg=as.data.frame(ggplot2::mpg)
head(mpg)
fuel <- data.frame(fl = c("c", "d", "e", "p", "r"),
                   price_fl = c(2.35, 2.38, 2.11, 2.76, 2.22),
                   stringsAsFactors = F)
fuel  # 출력

# 1
mpg_new <- left_join(mpg, fuel, by="fl")

# 2
mpg_new %>% 
  select(model, fl, price_fl) %>% 
  head(5)
