# 복습
library(ggplot2)
library(dplyr)
plot(diamonds$cut)
qplot(diamonds$cut)

# diamonds의 칼럼의 이름은?
colnames(diamonds)

# exam과 name을 한 프레임으로 결합시키시오.
exam <- data.frame(class= c(1,2,3,4,5),
                   subject = c("math", "english", "science","society","korean"))

name <-data.frame(class= c(1,2,3,4,5),
                  teacher = c("kim","lee","park","choi","jung")) 
total <- left_join(exam, name, by="class")
total

# summarise()는 항상 어떤 함수랑 같이 쓰이는가
group_by()

mpg
mpg %>% group_by(manufacturer) %>% summarise(avg = mean(cty)) 

mpg %>% 
  group_by(manufacturer) %>% 
  filter(class == "suv") %>% 
  summarise(count=n()) %>% 
  arrange(desc(count))

#mpg 수정한 경우
rm(mpg)
mpg <- mpg %>% select(model, year)

select(mpg, manufacturer, year)
mpg %>% select(manufacturer)

#순서 중요!
mpg %>% 
  filter(class == "suv") %>% 
  group_by(manufacturer) %>% 
  summarise(num_suv=n()) %>% 
  arrange(desc(num_suv))

pwd()
setwd("C:/Rdata")
exam <- read.csv("csv_exam.csv")
exam
exam$math2 <- ifelse(exam$math >= 80, "a",
                     ifelse(exam$math >= 60, 'b',
                            ifelse(exam$math >= 40, "c", "f"))) 

exam %>% mutate(math2 = ifelse(math>= 80, 
                               ifelse(math>= 60, "b", )))


#수업
#p.161~
df <- data.frame(gender = c("M", "F", NA, "M", "F"),
                 score = c(5,4,3,4,NA))
df  

#1. 결측치가 있는지
is.na(df)
sum(is.na(df))

table(is.na(df))

airquality
#1. 모두 몇개
sum(is.na(airquality))
#2. Ozone 칼럼에는 몇개?
sum(is.na(airquality$Ozone))

summary(mpg)
summary(airquality)

#2. 결측치 처리하기  
# - 삭제하기
na.omit(df)

- 채워넣기
df$gender <- ifelse(is.na(df$gender), "M", df$gender)
df

df$score <- ifelse(is.na(df$score), 5, df$score)  
df

df[3,1] <- "F"
df[3,1] <- NA
df[5,2] <- NA
df

summary(df)

mean(df$score, na.rm = T)

airquality %>% head

# Ozone의 평균값은?
airquality
mean(airquality$Ozone, na.rm = T)


is.na(df$score)
df %>% filter(!is.na(df$score)) %>% group_by(gender) %>% 
  summarise(mean(score))

airquality %>% head

# 1. Solar.R을 Solar로 바꾸어 주세요.
# 2. Ozone과 Solar만 행을 뽑아서,
#    두 칼럼 각각의 평균을 구해 주세요.
airquality<-airquality %>%
  rename(Solar=Solar.R)
colnames(airquality)  

airquality<-airquality%>%select(Ozone,Solar)
airquality
mean(airquality$Ozone,na.rm=T)
mean(airquality$Solar,na.rm=T)

#p. 171 ~
# 이상치 outlier
outlier <- data.frame(gender = c(1,2,1,3,2,1),
                      score = c(5,4,3,4,2,10))
outlier

#1. 이상치가 있는지?
boxplot(diamonds$cut)
ggplot(data = diamonds, aes(x = cut)) + geom_boxplot()

boxplot(outlier$score)


# 2. 이상치 처리하기

# 6번은 제외하고 평균score를 구해주세요.
mean(outlier$score[-6])

temp <- outlier %>% filter(score < 9)
mean(temp$score)

outlier %>% filter(score <9) %>% 
  summarise(mean(score))

#vector : 한줄, 1차원 vectorize(한 줄로 쭉 만든다.)
#dataframe : 표, 2차원

outlier$score[3]
outlier[3,2]

mean(outlier$score[-6])
outlier$score[-6]
outlier$score[1:5]
outlier$score[c(1,3,4)]


outlier

# 10을 결측치로 바꾸어 주세요.
outlier[6,2] <- NA
outlier$score[6] <- NA
outlier

mean(outlier$score, na.rm = T)

#p.180~
#ggplot2
#grammar of graphics

#ggplot2
#ggplot()

plot(mpg$displ, mpg$hwy) %>% data.frame

ggplot(data = mpg, mapping = aes(x=displ, y=hwy)) + 
  geom_point(color="blue", size=3) +
  geom_line() +
  xlim(3,6) + ylim(10,30)

ggplot(mpg, aes(x=displ, y=hwy)) + geom_point()

#aes : aesthetic
#geom : geometry

head(mpg,2)
str(mpg)
glimpse


ggplot(mpg, aes(x=cty, y=hwy)) + geom_point()
ggplot(mpg, aes(x=displ, y=hwy)) + geom_point()
ggplot(mpg, aes(x=drv, y=hwy)) + geom_point()

ggplot(mpg, aes(x=displ, y=hwy)) + geom_point()
ggplot(mpg, aes(x=displ, y=hwy, color)) + geom_point(color = "red")
ggplot(mpg, aes(x=displ, y=hwy, color=drv)) + geom_point(size=4)
ggplot(mpg, aes(x=displ, y=hwy, color=drv)) + geom_point(size=4)

#막대그래프: geom_bar()
ggplot(mpg, aes(x=class, fill=class)) + geom_bar(aes(fill="red"))
ggplot(mpg, aes(x=drv, fill=drv)) + geom_bar(aes(fill="red"))
ggplot(mpg, aes(x=drv, fill=class)) + geom_bar()
ggplot(mpg, aes(x=class, fill=drv)) + geom_bar()

df <- table(mpg$class) %>% as.data.frame()
df <- df %>% rename(class = Var1)

ggplot(df, aes(x= class, y= Freq)) + geom_bar(stat = "identity")
ggplot(df, aes(x= class, y= Freq)) + geom_col()

df1 <- df %>% arrange(desc(Freq)) %>% ggplot(aes(x=class, y=Freq, fill= class)) + geom_col()
ggplot(aes(x=reorder(class, Freq), y=Freq, fill=class))

ggplot(mpg, aes(x=displ, y=hwy)) + geom_col()

#drv: 구동방식별로 고속도로 평균연비를 구해서 그래프로 그려라.

df1 <- mpg %>% group_by(drv) %>% summarise(평균연비 = mean(hwy))
df1

ggplot(df1, aes(x=drv, y=평균연비, fill=drv)) + geom_col()
ggplot(df1, aes(x=reorder(drv, -평균연비), y=평균연비, fill=drv)) + geom_col()

economics$date <- as.character(economics$date)

ggplot(economics, aes(x=date, y= unemploy)) + geom_line()

economics$date <- as.Date(economics$date)
                                                                                           
ggplot(data=mpg, aes(x=drv)) + geom_boxplot()

ggplot(data=mpg, aes(x=drv, y=hwy, fill=drv)) 
geom_boxplot(diamonds)











 
  
