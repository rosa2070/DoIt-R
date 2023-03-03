#p.162~
#7-1
#결측치 찾기
df <- data.frame(sex = c("M", "F", NA, "M", "F"),
                 score = c(5, 4, 3, 4, NA))
df
is.na(df)
table(is.na(df))
table(is.na(df$sex))
table(is.na(df$score))

mean(df$score)
sum(df$score)

#결측치 제거하기
library(dplyr)
df %>% filter(is.na(score))

df %>% filter(!is.na(score))
df_nomiss <- df %>% filter(!is.na(score))
mean(df_nomiss$score)
sum(df_nomiss$score)

df_nomiss <- df %>% filter(!is.na(score) & !is.na(sex))
df_nomiss

df_nomiss2 <- na.omit(df)
df_nomiss2

# 함수의 결측치 제외 기능 이용하기
mean(df$score, na.rm = T)
sum(df$score, na.rm = T)

exam <- read.csv("csv_exam.csv")
exam[c(3,8,15), "math"] <- NA
exam

exam %>% summarise(mean_math = mean(math))

exam %>% summarise(mean_math = mean(math, na.rm = T))

exam %>% summarise(mean_math = mean(math, na.rm = T),
                   sum_math = sum(math, na.rm = T),
                   median_math = median(math, na.rm = T))

#평균값으로 결측치 대체하기
mean(exam$math, na.rm = T)

exam$math <- ifelse(is.na(exam$math), 55, exam$math)
table(is.na(exam$math))
exam

mean(exam$math)

# 퀴즈
mpg <- as.data.frame(ggplot2::mpg)
mpg[c(65, 124, 131, 153, 212), "hwy"] <- NA
#1
table(is.na(mpg$drv))
table(is.na(mpg$hwy))
#2
mpg %>% filter(!is.na(hwy)) %>%
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy)) %>% 
  arrange(desc(mean_hwy))

#7-2
# 이상치 제거하기 - 존재할 수 없는 값
outlier <- data.frame(sex = c(1,2,1,3,2,1),
                      score = c(5,4,3,4,2,6))
outlier

table(outlier$sex)
table(outlier$score)

outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex)
outlier

outlier$score <- ifelse(outlier$score > 5, NA, outlier$score)
outlier

outlier %>% 
  filter(!is.na(sex) & !is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(mean_score = mean(score))

boxplot(mpg$hwy)

boxplot(mpg$hwy)$stats

mpg <- as.data.frame(ggplot2::mpg)
mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)
table(is.na(mpg$hwy))

mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy, na.rm = T))

#퀴즈
mpg <- as.data.frame(ggplot2::mpg)
mpg[c(10, 14, 58, 93), "drv"] <- "k"
mpg[c(29,43,129,203), "cty"] <- c(3,4,39,42)
mpg

#1
mpg$drv <- ifelse(mpg$drv %in% c("f",4,"r"), mpg$drv, NA)
#2
boxplot(mpg$cty)$stats
mpg$cty <- ifelse(mpg$cty < 9 | mpg$cty > 26, NA, mpg$cty)
boxplot(mpg$cty)
#3
mpg %>% 
  group_by(drv) %>% 
  summarise(mean_cty = mean(cty, na.rm = T)) %>% 
  filter(!is.na(drv))

mpg %>% 
  filter(!is.na(drv) & !is.na(cty)) %>% 
  group_by(drv) %>% 
  summarise(mean_cty = mean(cty))

#p.181 ~
#8-2
library(ggplot2)
ggplot(data = mpg, aes(x=displ, y=hwy))

ggplot(data = mpg, aes(x=displ, y=hwy)) + geom_point()

ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point() + xlim(3, 6)

ggplot(data = mpg, aes(x=displ, y=hwy)) +
  geom_point() +
  xlim(3, 6) +
  ylim(10, 30)

#퀴즈
#1
ggplot(data = mpg, aes(x=cty, y = hwy)) + geom_point()
#2
midwest
ggplot(data=midwest, aes(x=poptotal, y=popasian)) + 
  geom_point() +
  xlim(0,500000) +
  ylim(0,10000)

#8-3
library(dplyr)
mpg <- as.data.frame(ggplot2::mpg)
df_mpg <- mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy))
df_mpg

ggplot(data = df_mpg, aes(x=drv, y=mean_hwy)) + geom_col()

ggplot(data = df_mpg, aes(x=reorder(drv, -mean_hwy), y=mean_hwy)) + geom_col()

#빈도 막대 그래프 만들기
ggplot(data = mpg, aes(x=drv)) + geom_bar()

ggplot(data = mpg, aes(x = hwy)) + geom_bar()

#퀴즈
#1
mpg <- as.data.frame(ggplot2::mpg)
df <- mpg %>% 
  filter(class == "suv") %>% 
  group_by(manufacturer) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  arrange(desc(mean_cty)) %>% 
  head(5)
ggplot(data = df, aes(x=reorder(manufacturer,-mean_cty), y=mean_cty)) + geom_col()
#2
ggplot(data = mpg, aes(x=class)) + geom_bar()

#8_4
# 시계열 그래프 만들기
ggplot(data = economics, aes(x=date, y=unemploy)) + geom_line()

#퀴즈
ggplot(data=economics, aes(x=date, y=psavert)) + geom_line()

#8-5
#상자 그림 만들기
ggplot(data=mpg, aes(x=drv, y=hwy)) + geom_boxplot()

#퀴즈
mpg_new <- mpg %>% filter(class %in% c("compact", "subcompact", "suv"))
ggplot(data=mpg_new, aes(x=class,y=cty)) + geom_boxplot()  
