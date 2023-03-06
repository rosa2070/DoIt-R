#15-1
#행 번호로 행 추출하기
exam <- read.csv("csv_exam.csv")

exam[]

exam[1,]
exam[2,]

#조건을 충족하는 행 추출하기
exam[exam$class == 1,]
exam[exam$math >= 80,]

exam[exam$class ==1 & exam$math >= 50,]
exam[exam$english < 90 | exam$science < 50, ]

#열 번호로 변수 추출하기
exam[,1]
exam[,2]
exam[,3]

#변수명으로 변수 추출하기
exam[,"class"]
exam[,"math"]
exam[,c("class", "math", "english")]

#행, 변수 동시 추출하기
exam[1,3]
exam[5,"english"]
exam[exam$math>=50, "english"]
exam[exam$math>=50, c("english", "science")]

exam$tot <- (exam$math + exam$english + exam$science)/3
aggregate(data=exam[exam$math >= 50 & exam$english >= 80,], tot~class, mean)

exam %>%  filter(math >= 50 & english >= 80) %>% 
  mutate(tot = (math + english + science)/3) %>% 
  group_by(class) %>% 
  summarise(mean = mean(tot))

#퀴즈
mpg <- as.data.frame(ggplot2::mpg)

mpg %>% 
  mutate(tot=(cty + hwy)/2) %>% 
  filter(class == "compact" | class=="suv") %>% 
  group_by(class) %>%
  summarise(mean_tot = mean(tot))

mpg$tot <- (mpg$cty + mpg$hwy)/2
mpg            
aggregate(data=mpg[mpg$class == "compact" | mpg$class == "suv",], tot~class, mean)            

#15-2
#변수 타입 간 차이 알아보기
var1 <- c(1,2,3,1,2)
var2 <- factor(c(1,2,3,1,2))
var1
var2

var1+2
var2+2

class(var1)
class(var2)

levels(var1)
levels(var2)

var3 <- c("a", "b", "b", "c")
var4 <- factor(c("a", "b", "b", "c"))

var3
var4

class(var3)
class(var4)

mean(var1)
mean(var2)

#변수 타입 바꾸기
var2 <- as.numeric(var2)
mean(var2)

class(var2)
levels(var2)

#퀴즈
#1
class(mpg$drv)
#2
mpg$drv <- as.factor(mpg$drv)
class(mpg$drv)
#3
levels(mpg$drv)

#15-3
a <- 1
a
b<-"hello"
b

class(a)
class(b)

x1 <- data.frame(var1=c(1,2,3), var=c("a", "b", "c"))
x1
class(x1)
x2 <- matrix(c(1:12), ncol=2)
x2
class(x2)
x3 <- array(1:20 ,dim = c(2,5,2))
x3
class(x3)

x4 <- list(f1 = a, 
           f2=x1,
           f3=x2,
           f4=x3)
x4
class(x4)
mpg <- ggplot2::mpg
x <- boxplot(mpg$cty)
x$stats[,1]
x$stats[,1][3]
x$stats[,1][2]

#pdf
#1.ggplot2 시작하기
str(mpg)
names(mpg)

ggplot(mpg, aes(x=displ, y=hwy))+ geom_point()
ggplot(mpg, aes(displ, cty, colour=class)) + geom_point()
ggplot(mpg, aes(displ, hwy)) + geom_point(aes(colour="blue"))
ggplot(mpg, aes(displ, hwy)) + geom_point(colour="blue")

ggplot(mpg, aes(displ, cty, colour=class)) + geom_point()
ggplot(mpg, aes(displ, cty, colour=trans)) + geom_point()
ggplot(mpg, aes(displ, cty, colour=drv)) + geom_point()
ggplot(mpg, aes(displ, cty, colour=cty)) + geom_point()


ggplot(mpg, aes(displ, cty, shape=drv)) + geom_point()
ggplot(mpg, aes(displ, cty, shape=class)) + geom_point()
ggplot(mpg, aes(displ, cty, shape=trans)) + geom_point()
ggplot(mpg, aes(displ, cty, shape=cty)) + geom_point()

ggplot(mpg, aes(displ, cty, size=cty)) + geom_point()
ggplot(mpg, aes(displ, cty, size=trans)) + geom_point()

ggplot(mpg, aes(displ, cty, size=cty)) + geom_point(colour="red")
ggplot(mpg, aes(displ, cty, size=cty)) + geom_point(colour=cty)
ggplot(mpg, aes(displ, cty, size=cty)) + geom_point(aes(colour=cty))

ggplot(mpg, aes(displ, cty, size=cty, color = drv)) +
  geom_point()

ggplot(mpg, aes(cty, hwy)) + geom_point()
ggplot(diamonds, aes(carat, price)) + geom_point()
ggplot(economics, aes(date, unemploy)) + geom_line()
ggplot(mpg, aes(cty)) + geom_histogram()
ggplot(mpg, aes(cty)) + geom_histogram(bins=20)

ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_wrap(~class)

#geom_* 요소 살펴보기
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth()

#블로그 참조 
#https://blog.naver.com/PostView.nhn?blogId=regenesis90&logNo=222203137784
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(method="lm")
#lm은 일직선형

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(method='loess')
  
ggplot(mpg, aes(drv,hwy)) +
  geom_point()

ggplot(mpg, aes(drv,hwy)) + geom_boxplot()

mpg %>% 
  filter(hwy < 20 & drv == "f")
mpg %>% 
  filter(hwy <25 & drv == "f") %>% arrange(hwy)

ggplot(mpg, aes(drv, hwy)) + geom_violin()

ggplot(mpg, aes(drv, hwy)) + geom_jitter()
#geom_jitter은 분산함수
#https://m.blog.naver.com/PostView.naver?isHttpsRedirect=true&blogId=coder1252&logNo=220986086164

ggplot(mpg, aes(drv, hwy, size=hwy)) +geom_point()
ggplot(mpg, aes(drv, hwy, size=hwy, color=hwy)) + geom_point()

ggplot(mpg, aes(hwy)) + geom_histogram()
ggplot(mpg, aes(hwy)) + geom_freqpoly()
# http://ds.sumeun.org/?p=970

ggplot(mpg, aes(hwy)) + geom_freqpoly(binwidth = 2.5)
ggplot(mpg, aes(hwy)) + geom_freqpoly(binwidth==1)

ggplot(mpg, aes(displ, colour=drv)) + geom_freqpoly(binwidth=0.5)
ggplot(mpg, aes(displ, fill=drv)) + geom_histogram(binwidth = 0.5)

ggplot(mpg, aes(displ, fill=drv)) + geom_histogram(binwidth=0.5, position="dodge")
# dodge는 복수의 데이터를 독립적인 막대 그래프로 나란히 표현
#https://m.blog.naver.com/coder1252/220931268317

ggplot(mpg, aes(displ, fill=drv)) + 
  geom_histogram(binwidth=0.5) +
  facet_wrap(~drv)

ggplot(mpg, aes(manufacturer)) + geom_bar()

drugs <- data.frame(drug = c("a", "b", "c"),
                    effect=c(4,9,6))
drugs
ggplot(drugs, aes(drug, effect)) + geom_bar(stat="identity")

ggplot(economics, aes(date, unemploy/pop)) +
  geom_line()
ggplot(economics, aes(date, uempmed)) + 
  geom_line()

economics

ggplot(economics, aes(date, unemploy/pop)) +
  geom_line()
ggplot(economics, aes(date, uempmed)) +
  geom_line()

#5. ggplot2 패키지를 알아보자
mtcars
str(mtcars)

mtcars$cyl <- as.factor(mtcars$cyl)

plot(mpg ~hp, data=mtcars,
     col=cyl, pch=c(4,6,8)[mtcars$cyl], cex=1.2)
#https://opentutorials.org/course/2074/13002

library(ggplot2)
ggplot(mtcars, aes(x=hp, y=mpg, color=cyl, shape=cyl)) +
  geom_point(size=3)

install.packages("ggplot2")
library(ggplot2)

ggplot(data=mpg, aes(x=displ, y=hwy))
ggplot(data=mpg, aes(x=displ, y= hwy)) + geom_point()
ggplot(data=mpg, aes(x=displ, y= hwy)) + geom_point() + xlim(3,6)
ggplot(data=mpg, aes(x=displ, y= hwy)) + geom_point() + xlim(3,6) +ylim(10,30)

str(mpg)
head(mpg)
ggplot(data=mpg, aes(x=displ, y=hwy, color=cty)) +geom_point(size=2)
ggplot(data=mpg, aes(x=displ, y=hwy, color=drv)) + geom_point(size=2)

dplyr::glimpse(mpg)
#https://statools.tistory.com/191

head(mpg)
ggplot(data=mpg, aes(x=displ,y=hwy,color=drv,shape=drv)) +
  geom_point(size=2)

ggplot(data=mpg, aes(x=displ,y=hwy,color=drv,shape=drv)) + 
  geom_point(size=2) +
  geom_smooth(method="lm")

p2 <- ggplot(data=mpg,
             aes(x=displ, y=hwy,color=drv,shape=drv)) +
  geom_point(size=2)
p2
p2 +geom_smooth(method="lm")
p2 + geom_smooth(method="lm") +
  theme_dark()

p3 <- ggplot(data=mpg,
             aes(x=displ, y=hwy,color=drv,shape=drv)) +
  geom_point(size=2) +
  geom_smooth(method="lm")

p3 + theme_dark()
p3 + theme_bw()
p3 + theme_classic()

p3 + labs(title="<배기량에 따른 고속도로 연비 비교 >", x="배기량", y="연비")
p3 + facet_wrap(~drv)
p3 + facet_wrap(~class)
#https://m.blog.naver.com/regenesis90/222199989285