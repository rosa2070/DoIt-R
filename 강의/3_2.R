# p.124~
dplyr

exam <- read.csv("csv_exam.csv")
#행
filter(exam, class == 1)
filter(exam, class == 2)
filter(exam, class != 2) # not equal
#영어가 90이상인 학생은 몇 명?
filter(exam, english >= 90)

# 영어가 90이상이고 1반 학생은?
filter(exam, english >= 90 & class == 1)

exam %>% filter(class == 1)
exam %>% filter(english > 90) %>% filter(class == 1)

#수학이 90보다 작은 학생들 중에서 
#영어는 70보다 크고,
#1반이 아닌 학생들?
exam %>% filter(math < 90) %>% filter(english > 70) %>% filter(class != 1)

#1반 또는 2반 또는 3반 학생들?
exam %>% filter(class == 1 | class == 2)
exam %>% filter(class %in% c(1,2,3))

# 1반 학생의 수학평균은?
mean((exam %>% filter(class == 1))$math)

# 열 뽑기

head(exam, 3) 
exam %>% head(2)

select(exam, class, math)

exam %>% select(class, math)
exam %>% select(-math)
exam %>% select(-math, -english)

exam %>% filter(class == 1) %>% select(math, english)
exam %>% select(math, english) %>% filter(class == 1)
exam %>%  select(math, english, class) %>% filter(class == 1)

#정렬 arrange()

exam %>% arrange(math)
exam %>% arrange(desc(math))

exam %>% arrange(desc(class), math)

# 칼럼 추가하기

exam %>% mutate(총점 = math + english + science)
# 총점과 평균 칼럼을 추가해 주세요
exam %>% mutate(total = math + english + science, avg = total/3) %>%select(total, avg)

#ifelse(조건, 참일 때, 거짓일 때)

exam %>% mutate(합격여부 = ifelse(science > 60, "합격", "불합격"))

#수학, 영어, 과학의 "평균"칼럼을 구하고, 
# 평균이 60 이상이면 "PASS", 그렇지 않으면 "FAIL"을
# 표시하는 "통과여부" 칼럼을 구하고
# 평균의 내림차순으로 정렬합시다.

exam %>% mutate(avg = (math + english + science)/ 3, 
                passOrNot = ifelse(avg >= 60, "PASS", "FAIL")) %>% arrange(desc(avg))

filter
select
arrange
mutate
summarise()
group_by()

library(readxl)
library(dplyr)
exam <- read_excel("excel_exam.xlsx")
head(exam)

class1 <- exam %>% filter(class == 1)
mean(class1$math)

class2 <- exam %>% filter(class == 2)
mean(class2$math)

class3 <- exam %>% filter(class == 3)
mean(class3$math)

class4 <- exam %>% filter(class == 4)
mean(class4$math)

exam %>%  group_by(class) %>% 
  summarise(최고점=max(math))

exam %>% select(math) %>% summarise(평균=mean(math))

#요약함수
#sum, mean, sd, median, min, max, n()

exam %>% group_by(class) %>% 
  summarise(n())


#영어의 반별 평균, 최고점, 최하점, 중앙값, 빈도수 
exam %>% group_by(class) %>% summarise(avg=mean(english),
                                       max=max(english),
                                       min=min(english),
                                       mid=median(english),
                                       n = n())

diamonds %>% head()
mpg

table( c("A", "A", "B"))
table(diamonds$cut)
unique(diamonds$color)
# cut 별로 가격의 최댓값과 최솟값은?
diamonds %>%  group_by(cut) %>%  summarise(max=max(price),
                                           min=min(price))

str(diamonds)

diamonds %>%  group_by(carat) %>%  summarise(max=max(price),
                                           min=min(price))

rownames(mpg)
colnames(mpg)
names(mpg)
mpg$class

#p.150~
#퀴즈
mpg %>% group_by(class) %>% summarise(avg=mean(cty))

#내림차순으로 정렬
mpg %>% group_by(class) %>% summarise(avg=mean(cty)) %>% arrange(desc(avg))

#''
#``: backtick
#~ : tilde
#^ : carat

# hwy 연비의 평균이 좋은 상위 6개 회사
mpg %>% group_by(manufacturer) %>% 
  summarise(MEAN = mean(hwy)) %>% 
  arrange(desc(MEAN)) %>% 
  select(manufacturer, MEAN) %>% head

# hwy 연비의 평균이 하위 6개 회사
mpg %>% group_by(manufacturer) %>% 
  summarise(MEAN = mean(hwy)) %>% 
  arrange(MEAN) %>% 
  select(manufacturer, MEAN) %>% head

mpg %>% select(year, class) %>% head(20) 

mpg %>% select(manufacturer, class) %>% 
  filter(class == 'compact') %>% 
  group_by(manufacturer) %>% 
  summarise(빈도수 = n()) %>% 
  arrange(desc(빈도수))

#p.152~
test1 <- data.frame(id = c(1,2,3,4,5,6), 
           midterm = c(60, 80, 70, 90, 85, 100))
test2 <- data.frame(id = c(1,2,3,4,5,7),
                    final = c(70, 83, 65, 95, 80, 0))       

left_join(test1, test2, by='id')
left_join(test1, test2)          
right_join(test1, test2, by = 'id')
left_join(test2, test1)

full_join(test1, test2)

test1          
test2          

bind_cols(test1, test2) %>% select(-id...3)
bind_rows(test1, test2) 

# NA ( Not Available ) 결측치

exam

name <- data.frame(class=c(1,2,3,4,5),
                   teacher=c("Lee", "Park", "Jeong", "choi", "Kim"))
left_join(exam, name, by='class')

# 칼럼 이름 바꾸기
# dplyr: filter, select, rename, arrange, mutate, group_by, summarise(), left_join, bind_rows

exam %>%  rename( 반 = class)

mpg %>% rename( city = cty, highway = hwy) %>%  names()
mpg

mpg %>% names()

colnames(mpg)

# manufacturer를 mf로 바꾸어주세요
colnames(mpg) <- c("mf", "model", "city")

mpg$mf <- mpg$model
mpg

exam <- exam %>% filter(id < 11)
exam <- exam %>% select(id, class, math)
exam
exam$class <- exam$math
exam

exam -> exam %>% rename(english = classes)
exam
# SUM 칼럼을 만들어주세요.
exam$SUM <- exam$math + exam$english
exam

#~p.160