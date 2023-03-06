#복습
setwd("C:/Rdata")
exam <- read.csv("csv_exam.csv")
exam

#1. 영어가 70이하이고, 4반, 5반인 학생?
library(dplyr)
exam %>% filter(english <= 70 & class %in% c(4,5))

#모든 클래스 세과목 평균
exam %>% group_by(class) %>% summarise(mean(math+english+science))

# mpg에서 도심연비가 좋은 차종 상위 6개는?
library(ggplot2)
mpg %>% group_by(model) %>% summarise(MEAN=mean(cty)) %>% 
  arrange((MEAN)) %>% head(6)

#영어가 90점 이상이고 1반인 학생?
exam %>% filter(english >= 90 & class == 1)

#수학이 90점보다 작은 학생들 중에서 영어는 70보다 크고, 1반이 아닌 학생들은?
exam %>% filter(math < 90 & english >70 & class != 1)

#총점(total)과 평균(avg) 칼럼을 추가해주세요
exam %>% mutate(total = math + english + science, avg = total/3)

# 통과여부 칼럼추가
exam %>% mutate(통과여부 = ifelse((math+english+science)/3 >= 60, "PASS", "FAIL"))

#그래프 만들기
library(ggplot2)
ggplot(data = mpg, aes(x=class)) + geom_bar(fill="skyblue",color="red")

#그래프2
ggplot(data=mpg, aes(x=class, fill=class)) +geom_bar()

mpg2 <- mpg %>% group_by(manufacturer) %>% summarise(mean_cty = mean(cty)) %>% arrange(desc(mean_cty))%>% head(5)
ggplot(data=mpg2, aes(x=reorder(manufacturer,-mean_cty), y=mean_cty)) + geom_col()

#수업
#filter, select

exam <- read.csv("csv_exam.csv")

exam[,c("class", "english")]
exam[,c(2,3)]

exam[c(2,3),]
exam[3:5, ]

exam[exam$english>90,]
exam[exam$class==1,]
exam[,]

exam$class==1
exam$english > 90

#A, B, C, D, E, F, G, H
#학점: A, B
#범주, 카테고리, factor: 금, 은, 동

var1 <- c(1,2,3,4,5)
mode(var1)
var2 <- as.factor(var1)
class(var2)
mode(var2)
str(mpg)
str(diamonds)
c("a","b", "b", "c","c", "c")
class(var3)  

#dataframe
#matrix
#array 배열

class(mpg)
class(mpg[1:3, 1:3])
class(mpg[,3])
class(mpg[3,])

class(mpg[,1])
class(mpg$manufacturer)

library(ggplot2)
ggplot(mpg, aes(x=manufacturer,y=class, color=drv)) + geom_point(size=3)
ggplot(data=mpg, aes(x=displ, y=hwy, color=class)) + geom_point(size=2)
ggplot(data=mpg, aes(x=displ, y=hwy, color=cty)) + geom_point(size=2)

ggplot(data=mpg, aes(x=displ, y=hwy, color=cty, shape=cty)) + geom_point(size=3)
ggplot(data=mpg, aes(x=displ, y=hwy)) + 
  geom_point(size=3) + 
  geom_smooth(method= "lm")

g1 <- ggplot(data=mpg, aes(x=displ, y=hwy, color=drv, shape=drv)) + 
  geom_point(size=2) + geom_smooth(method= "lm")
g1
g1 + theme_economist()

g1 + theme_wsj()
g1 + labs(title="< 배기량에 따른 고속도로 연비 비교 >", x="배기량", y="연비")
g1 
  
g1 + facet_wrap(~class)

economics
ggplot(economics, aes(date, uneconomy)) + geom_line()
ggplot(mpg, aes(cty)) + 
  geom_histogram(bins=10) + 
  geom_freqpoly(bins=20,color='red')

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(~class)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(method = 'loess')

ggplot(mpg, aes(drv, hwy)) +
  geom_point()

ggplot(mpg, aes(drv, hwy)) + geom_violin()
ggplot(mpg, aes(drv, hwy)) + geom_jitter()

ggplot(mpg, aes(hwy)) + geom_histogram()
ggplot(mpg, aes(hwy)) + geom_freqpoly()

ggplot(mpg, aes(x=displ, color=drv)) +
  geom_freqpoly(binwidth = 0.5, size=2)

ggplot(mpg, aes(displ, fill = drv)) +
  geom_histogram(binwidth = 0.5, position = "dodge")

ggplot(mpg, aes(displ, fill = drv)) + 
  geom_histogram(binwidth = 0.5) +
  facet_wrap(~drv)

ggplot(mpg, aes(manufacturer, fill = manufacturer))  + geom_bar()

drugs <- data.frame(drug = c("a", "b", "c"),
                    effect = c(4,9,6))
ggplot(drugs, aes(drug, effect)) + geom_bar(stat = "identity")

economics

ggplot(economics, aes(date, unemploy/pop)) +
  geom_line()

library(foreign)
setwd("C:/Rdata")
raw_welfare <- read.spss(file = "Koweps_hpc10_2015_beta1.sav")
welfare <- as.data.frame(raw_welfare)
dim(welfare)
str(welfare)
summary(welfare)
head()

welfare <- welfare %>% 
  select(gender = h10_g3, birth=h10_g4, 
         marriage = h10_g10, religion=h10_g11,
         income = p1002_8aq1, job = h10_eco9,
         region=h10_reg7)
welfare

head(welfare)

class(welfare)
welfare <- as_tibble(welfare)
welfare

mpg
diamonds
air <- as_tibble(airquality)
air

boxplot(welfare)
sum(is.na(welfare))
colSums(is.na(welfare))
summary(welfare$income)

mean(welfare$income)
mean(welfare$income, na.rm = T)

range(welfare$income, na.rm = T)
welfare$income <- ifelse(welfare$income == 0, NA, welfare$income)
summary(welfare$income)
range(welfare$income, na.rm = T)

boxplot(welfare$income)
plot(welfare$income)
ggplot(data=welfare, aes(x=income)) + geom_density()
str(welfare)       
str(welfare) <- as.factor(welfare$gender)

welfare$gender
#male, female
welfare
welfare$gender <- ifelse(welfare$gender == "1", "male", "female")
welfare
ggplot(data=welfare, aes(x=gender)) + geom_bar()

#여자평균소득, 남자평균소득
mean_income <- welfare %>% 
  group_by(gender) %>% 
  summarise(avg_income = mean(income, na.rm = T))
ggplot(mean_income, aes(x=gender, y=avg_income, fill = gender)) + geom_col()

welfare %>% ggplot(aes(x=income, color=gender)) + geom_density()

max(welfare$birth)

#2014 -> 1살
#2013 -> 2살
#2012 -> 3살

welfare$age <- 2015-welfare$birth
range(welfare$age)

welfare %>% group_by(age) %>% 
  summarise(평균소득 = mean(income, na.rm=T)) %>% 
  ggplot(aes(x=age, y=평균소득)) + geom_line()
xlim(20, 80)
