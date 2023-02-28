
# 데이터프레임

english <- c(90, 80, 60, 70)
math <- c(50, 60, 100, 20)


rm(str1)

rm(str, str2)

rm(list = ls())

df_midterm <- data.frame(english, math)

class <- c(1,1,2,2)
df_midterm <- data.frame(english, math, class)
df_midterm

# 접근하기

df_midterm[2,3]
df_midterm[3,2]
df_midterm[3,]
df_midterm[3, c(1,2,3)]
df_midterm[2:3, 1:2]
df_midterm[c(2,3),c(1,2)]

df_midterm[3, -1]
df_midterm[3, -2]
df_midterm[3, -3]
df_midterm[3, 1:2]

df_midterm[ , 1]

df_midterm[ , 'english']
df_midterm$english
df_midterm$math
# 영어성적의 평균은?
mean(df_midterm[ , 'english'])
mean(df_midterm$english)

제품 <- c('사과', '딸기', '판매량')
가격 <- c(1800, 1500, 3000)
판매량 <- c(24, 38, 13)

df=data.frame(제품= c('사과', '딸기', '판매량'), 
                       가격= c(1800, 1500, 3000), 
                       판매량= c(24, 38, 13))


df
mean(df$판매량)

excel
csv

# 함수: 입력,,,출력
# 파일 위치 
# 함수이름
read_excel("excel_exam.xlsx")
library(readxl)
install.packages("readxl")


#working directory
getwd()
setwd("C:/Rdata")



df <- read_excel("excel_exam.xlsx, sheet=2")


write.csv(df, "exam.csv")

# 영어평균은?
mean(df$english)

# csv 파일 읽기

read.csv("csv_exam.csv")
exam <- read.csv("exam.csv")
exam[  ,2:6 ]
exam <- exam[ , -1]

exam

# csv 파일이란
comma separated value


df


saveRDS(df, file = "df.rds")
readRDS("df.rds")


save(df, file = "df1.rds")
readRDS("df.rds")



exam
head(exam)
head(exam, 2)

tail(exam)
tail(exam, 2)


View(exam)
dim(exam)



str(exam)  #structure
# obs: observations (관측치)
# variables: 변수

exam

20 X 5
행     열
관측치 변수


int : integer(정수)

str(exam)
summary(exam)

mpg

diamonds
library(ggplot2)
# 몇행 몇 열입니까?
dim
# carat의 최댓값은?
# price의 중앙값은?
# price의 평균은?


  


















































