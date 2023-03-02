#p.108 ~
str(mpg)
summary(mpg)

#5-2
#변수명 바꾸기
df_raw <- data.frame(var1 = c(1,2,1),
                     var2 = c(2,3,2))
df_raw

install.packages("dplyr")
library(dplyr)

df_new <- df_raw
df_new

df_new <- rename(df_new, v2 = var2)
df_new

# 퀴즈
#1
mpg <- as.data.frame(ggplot2::mpg)
mpg_new <- mpg
#2
mpg_new <- rename(mpg_new, city=cty, highway=hwy)
#3
head(mpg_new)

#5-3
#변수 조합해 파생변수 만들기
df <- data.frame(var1 = c(4,3,8),
                 var2 = c(2,6,1))
df
df$var_sum <- df$var1 + df$var2
df$var_mean <- (df$var1 + df$var2)/2
df

#mpg 통합 연비 함수 만들기
mpg$total <- (mpg$cty + mpg$hwy)/2
head(mpg)

mean(mpg$total)

#조건문을 활용해 파생변수 만들기
summary(mpg$total)
hist(mpg$total)

mpg$test <- ifelse(mpg$total >= 20, "pass", "fail")
head(mpg, 20)
table(mpg$test)

library(ggplot2)
qplot(mpg$test)

# 중첩 조건문 활용하기
mpg$grade <- ifelse(mpg$total >= 30, "A",
                    ifelse(mpg$total >= 20, "B", "C"))
head(mpg, 20)

table(mpg$grade)
qplot(mpg$grade)                    

mpg$grade2 <- ifelse(mpg$total >= 30, "A",
                     ifelse(mpg$total >= 25, "B",
                            ifelse(mpg$total >=20, "C", "D")))

#분석도전!
# 아직안함

# p. 124~
# 조건에 맞는 데이터만 추출하기
library(dplyr)
exam <- read.csv("csv_exam.csv")
exam

exam %>% filter(class == 1)
exam %>% filter(class != 1)

# 초과, 미만, 이상, 이하 조건 걸기
exam %>% filter(math > 50)
exam %>% filter(math < 50)
exam %>% filter(english >= 80)
exam %>% filter(english <= 80)

# 여러 조건을 충족하는 행 추출하기
exam %>% filter(class == 1 & math >= 50)

getwd()
setwd("C:/Rdata")

exam %>% filter(class == 2 & english >= 80)

# 여러 조건 중 하나 이상 충족하는 행 추출하기
exam %>% filter(math >= 90 | english >= 90)
exam %>% filter(english < 90 | science < 50)

exam %>% filter(class == 1 | class ==3 | class == 5)
exam %>% filter(class %in% c(1,3,5))

class1 <- exam %>% filter(class == 1)
class2 <- exam %>% filter(class == 2)

mean(class1$math)
mean(class2$math)

#퀴즈
#1
mpg <- as.data.frame(ggplot2::mpg)
mpg_a <- mpg %>% filter(displ <= 4)
mpg_b <- mpg %>% filter(displ >= 5)
mean(mpg_a$hwy)
mean(mpg_b$hwy)
#2
mpg
mpg_audi <- mpg %>% filter(manufacturer == "audi")
mpg_toyota <- mpg %>% filter(manufacturer == "toyota")
mean(mpg_audi$cty)
mean(mpg_toyota$cty)
#3
mpg_new <- mpg %>% filter(manufacturer %in% c("chevrolet", "ford", "honda"))
mean(mpg_new$hwy)

#변수 추출하기
exam %>% select(math)
exam %>% select(english)
exam %>% select(class, math, english)
exam %>% select(-math)
exam %>% select(-math, -english)

#dplyr 함수 조합하기
exam %>% 
  filter(class == 1) %>% 
  select(english)

exam %>% 
  select(id, math) %>% 
  head

exam %>% 
  select(id, math) %>% 
  head(10)

#퀴즈
#1
mpg <- as.data.frame(ggplot2::mpg)
df <- mpg %>% select(class, cty) 
head(df)
#2
df_suv <- df %>% filter(class=="suv")
df_compact <- df %>% filter(class == "compact")
mean(df_suv$cty)
mean(df_compact$cty)

# 오름차순으로 정렬하기
exam %>% arrange(math)

# 내림차순으로 정렬하기
exam %>% arrange(desc(math))
exam %>% arrange(class, math)

# 퀴즈
mpg <- as.data.frame(ggplot2::mpg)
mpg %>% filter(manufacturer == "audi") %>% 
  arrange(desc(hwy)) %>% 
  head(5)

#파생변수 추가하기
exam %>% 
  mutate(total = math + english + science) %>% 
  head

exam %>% 
  mutate(total = math + english + science,
                mean = (math + english + science)/3) %>%
  head

exam %>% 
  mutate(test = ifelse(science >= 60, "pass", "fail" )) %>% 
  head

exam %>% mutate(total = math + english + science) %>% 
  arrange(total) %>% 
  head

# 퀴즈
#1
mpg <- as.data.frame(ggplot2::mpg)
mpg_new <- mpg

mpg_new <- mpg_new %>% mutate(total = cty + hwy)
#2
mpg_new <- mpg_new %>% mutate(mean=total/2)
#e
mpg_new %>% 
  arrange(desc(mean)) %>% 
  head(3)
#4
mpg %>% 
  mutate(total = cty + hwy,
         mean = total/2) %>% 
  arrange(desc(mean)) %>% 
  head(3)



#6-6
#집단별로 요약하기
exam %>% summarise(mean_math = mean(math))

exam %>% 
  group_by(class) %>% 
  summarise(mean_math = mean(math))

exam %>% 
  group_by(class) %>% 
  summarise(mean_math = mean(math),
            sum_math = sum(math),
            median_math = median(math),
            n = n())

mpg %>% 
  group_by(manufacturer, drv) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  head(10)

# dplyr 조합하기
mpg %>% 
  group_by(manufacturer) %>% 
  filter(class == "suv") %>% 
  mutate(tot = (cty+hwy)/2) %>% 
  summarise(mean_tot = mean(tot)) %>% 
  arrange(desc(mean_tot)) %>% 
  head(5)

#퀴즈
#1
mpg <- as.data.frame(ggplot2::mpg)

mpg %>% 
  group_by(class) %>% 
  summarise(mean_cty = mean(cty))
#2
mpg %>% 
  group_by(class) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  arrange(desc(mean_cty))
#3
mpg %>% 
  group_by(manufacturer) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  arrange(desc(mean_cty)) %>% 
  head(3)
#4
mpg %>% 
  filter(class == "compact") %>% 
  group_by(manufacturer) %>%
  summarise(count = n()) %>% 
  arrange(desc(count))

#6-7
#가로로 합치기
test1 <- data.frame(id = c(1,2,3,4,5),
                    midterm = c(60, 80, 70, 90, 85))
test2 <- data.frame(id=c(1,2,3,4,5),
                    final= c(70, 83, 65, 95, 80))  
test1
test2

total <- left_join(test1, test2, by = "id")
total

#다른 데이터를 활용해 변수 추가하기
name <- data.frame(class = c(1,2,3,4,5),
                   teacher = c("kim", "lee", "park", "choi", "jung"))
name

exam_new <- left_join(exam, name, by="class")
exam_new

# 세로로 합치기
group_a <- data.frame(id=c(1,2,3,4,5),
                      test=c(60,80,70,90,85))
group_b <- data.frame(id=c(6,7,8,9,10),
                      test = c(70, 83, 65, 95, 80))
group_a
group_b

group_all <- bind_rows(group_a, group_b)
group_all

# 퀴즈
fuel <- data.frame(fl = c("c", "d", "e", "p", "r"),
                   price_fl = c(2.35, 2.38, 2.11, 2.76, 2.22))
fuel

#1
mpg <- as.data.frame(ggplot2::mpg)
mpg <- left_join(mpg, fuel, by="fl")
#2
mpg %>% 
  select(model, fl, price_fl) %>% 
  head(5)

#분석도전!
#1
midwest <- as.data.frame(ggplot2::midwest)
midwest <- midwest %>%  mutate(ratio_child = (poptotal - popadults)/poptotal*100)
#2
midwest %>% 
  arrange(desc(ratio_child)) %>%
  select(county, ratio_child)
  head(5)
#3
midwest <- midwest %>% 
  mutate(grade = ifelse(ratio_child>=40, "large", ifelse(ratio_child>=30, "middle", "small")))
table(midwest$grade)
#4
midwest %>% 
  mutate(ratio_asian = (popasian/poptotal)*100) %>% 
  arrange(ratio_asian) %>% 
  select(state, county, ratio_asian) %>% 
  head(10) 

