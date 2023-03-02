# 변수 만들기
a <- 1
a

# 여러 값으로 구성된 변수 만들기
var1 <- c(1,2,5,7,8)
var1

var2 <- c(1:5)
var2

var3 <- seq(1,5)
var3

var4 <- seq(1, 10, by =2)
var4

var1
var1 + 2

var1
var2
var1 + var2

# 문자로 된 변수 만들기
str1 <- "a"
str1

str2 <- "Hello World!"
str2

str4 <- c("a", "b", "c")
str4

str5 <- c("Hello!", "World", "is", "good!")
str5

#숫자를 다루는 함수 이용하기
x <- c(1,2,3)
x
mean(x)
max(x)
min(x)

#문자를 다루는 함수 이용하기
str5
paste(str5, collapse = ",")
paste(str5, collapse = " ")

x_mean <- mean(x)
x_mean

str5_paste <- paste(str5, collapse = " ")
str5_paste

# ggplot2 패키지 설치하기
install.packages("ggplot2")
library(ggplot2)

x <- c("a", "a", "b", "c")
x
qplot(x)

?qplot

# 퀴즈
score = c(80, 60, 70, 50, 90)
meanscore = mean(score)

# 중간에 넣기


# exam 데이터 파악하기

exam <- read.csv("csv_exam.csv")
head(exam)
head(exam, 10)

tail(exam)
tail(exam, 10)

View(exam)

dim(exam)

str(exam)

summary(exam)

# mpg 데이터 파악하기
install.packages("ggplot2")
mpg <- as.data.frame(ggplot2::mpg)

head(mpg)
tail(mpg)
View(mpg)
dim(mpg)
str(mpg)
summary(mpg)
