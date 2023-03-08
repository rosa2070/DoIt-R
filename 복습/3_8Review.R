#크롤링
#https://brunch.co.kr/@dugi/12

library(rvest)
library(stringr)
library(dplyr)

title = c()
body = c()

url_1 <- "https://search.daum.net/search?w=news&DA=PGD&enc=utf8&cluster=y&cluster_page=1&q=%EB%A5%B4%EC%84%B8%EB%9D%BC%ED%95%8C&p="

for(i in 1:10) {
  #주소 쪼개서 합치기
  url <- paste(url_1, i, sep="")
  print(url)
  
  #웹페이지 주소로 이동
  hdoc <- read_html(url)
  
  #클래스 이름 잡기(html_nodes) -> 클래스에서 글자만 뽑아내기(html_text())
  title_part <- hdoc %>% html_nodes(".tit_main.fn_tit_u") %>%  html_text()
  body_part <- hdoc %>% html_nodes(".desc") %>% html_text()
  
  title <- c(title, title_part)
  body <- c(body, body_part)
  
}

#news = data.frame(제목 = title, 본문 = body)
title %>% head()
news %>% head()

#cbind= 열 결합
news = cbind(title, body)
news

write.csv(news, "르세라핌.csv")

#빈도 수 상위 20개를 wordcloud로 만들자.
library(KoNLP)

useNIADic()
useSejongDic()

lesserafim <- read.csv("르세라핌.csv", header=T, fileEncoding = "UTF-8")

#특수문자 제거
lesserafim <- str_replace_all(lesserafim, "\\W", " ")

#명사 추출. extractNoun은 출력 결과를 리스트 형태로 반환
nouns <- extractNoun(lesserafim)

words <- unlist(nouns)
words <- words[nchar(words) >= 2]
df <- as.data.frame(table(words))
head(df)
df <- df %>%  arrange(desc(Freq)) %>% head(20)

library(wordcloud2)
wordcloud2(df)

#Sample
x <- 1:12
sample(x)
x <- 1:5
sample(x, 3)
sample(x, 10, replace=TRUE)

set.seed(1234)
sample(1:10, 5)

setwd("C:/RData")
exam <- read.csv("csv_exam.csv")
exam <- exam[,-1]
exam[seq(1,20,2),]

# 열 구하기
# dim은 행, 열을 셈
dim(exam)[2]
length(names(exam))

exam[1:10,]
exam[seq(1,20,2),]

#diamonds에서 5394개 뽑기
library(ggplot2)
diamonds
rm(diamonds)
diamonds[sample(53940, 5394),]
#nrow는 행의 수 출력
num <- sample(nrow(diamonds), nrow(diamonds)*.1)
dia <- diamonds[num,]

dia_2 <- diamonds[-num,]
dia_2

ggplot(dia, aes(x=carat, y=price, color=color)) + geom_point()

#diamonds를 5:3:2 비율로 3개의 서로 다른 세트 만들기
num <- sample(nrow(diamonds), nrow(diamonds)*.5)
train <- diamonds[num,]
nrow(train)

temp <- diamonds[-num,]
num1 <- sample(nrow(temp), nrow(diamonds)*.3)
val <- temp[num1,]
nrow(val)

test <- temp[-num1,]
test
nrow(test)

#diamonds를 4:3:2:1 비율로 4개의 서로 다른 세트 만들기
num1 <- sample(nrow(diamonds), nrow(diamonds)*.4)
set1 <- diamonds[num1,]
nrow(set1)

#noSet1: set1제외한 나머지
noSet1 <- diamonds[-num1,]
num2 <- sample(nrow(noSet1), nrow(diamonds)*.3)
set2 <- noSet1[num2,]
nrow(set2)

#noSet1Set2: set1, set2 제외한 나머지
noSet1Set2 <- noSet1[-num2,]
num3 <- sample(nrow(noSet1Set2), nrow(diamonds)*.2)
set3 <- noSet1Set2[num3,]
nrow(set3)

set4 <- noSet1Set2[-num3,]
nrow(set4)

#fraction= 분수, 비율
library(dplyr)
sample_n(diamonds, nrow(diamonds)*0.5)
sample_frac(diamonds, 0.5)
sample(c("A", "B"), 7, replace = T, prob=c(5,2))

ggplot(mpg, aes(x=drv, fill=drv)) +
  geom_bar() +
  scale_fill_hue(c=100)

ggplot(mpg, aes(x=drv, fill=drv)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set5")

#11-1
#미국 주별 강력 범죄율 단계 구분도 만들기
install.packages("mapproj")
install.packages("ggiraphExtra")
library(ggiraphExtra)

str(USArrests)
head(USArrests)

library(dplyr)
library(tibble)
crime <- rownames_to_column(USArrests, var="state")
crime$state <- tolower(crime$state)

str(crime)

install.packages("maps")
library(ggplot2)
states_map <- map_data("state")
str(states_map)

ggChoropleth(data = crime, 
             aes(fill= Murder,
                 map_id = state),
             map = states_map)

ggChoropleth(data = crime,
             aes(fill = Murder,
                 map_id=state),
             map = states_map,
             interactive = T)

#11-2
#대한민국 시도별 인구 단계 구분도 만들기
install.packages("stringi")

install.packages("devtools")
devtools::install_github("cardiomoon/kormaps2014")

library(kormaps2014)

str(korpop1)

library(dplyr)
korpop1 <- rename(korpop1, pop= 총인구_명, name=행정구역별_읍면동)

str(kormap1)

ggChoropleth(data = korpop1,
             aes(fill = pop,,
                 map_id=code, 
                 tooltip = name),
             map = kormap1,
             interactive = T)

#대한민국 시도별 결핵 환자 수 단계 구분도 만들기
str(tbc)

ggChoropleth(data = tbc,
             aes(fill = NewPts,
                 map_id=code,
                 tooltip = name),
             map=kormap1,
             interactive = T)

#12-1
#인터랙티브 그래프 만들기
install.packages("plotly")
library(plotly)

library(ggplot2)
p <- ggplot(data=mpg, aes(x=displ, y=hwy, col=drv)) + geom_point()
ggplotly(p)

p <- ggplot(data=diamonds, aes(x=cut, fill=clarity)) +
  geom_bar(position="dodge")
ggplotly(p)

#12-2
#인터랙티브 시계열 그래프 만들기
install.packages("dygraphs")
library(dygraphs)

economics <- ggplot2::economics
head(economics)

library(xts)
eco <- xts(economics$unemploy, order.by=economics$date)
head(eco)

dygraph(eco)

dygraph(eco) %>% dyRangeSelector()

eco_a <- xts(economics$psavert, order.by = economics$date)
eco_b <- xts(economics$unemploy/1000, order.by=economics$date)

eco2 <- cbind(eco_a, eco_b)
colnames(eco2) <- c("psavert", "unemploy")
head(eco2)

dygraph(eco2) %>% dyRangeSelector()

#10-1
#텍스트 마이닝 준비하기
library(KoNLP)

useNIADic()
useSejongDic()

#국정원 트윗 텍스트 마이닝
setwd("C:/RData")
twitter <- read.csv("twitter.csv",
                    header=T,
                    fileEncoding = "UTF-8")

twitter <- rename(twitter,
                  no=번호,
                  id=계정이름,
                  date=작성일,
                  tw=내용)

library(stringr)
twitter$tw <- str_replace_all(twitter$tw, "\\W"," ")
head(twitter$tw)

nouns <- extractNoun(twitter$tw)
wordcount <- table(unlist(nouns))
wordcount
df_word <- as.data.frame(wordcount)
df_word
df_word <- rename(df_word, word = Var1, freq= Freq)

df_word <- filter(df_word, nchar(word) >= 2)

#이후에 더 있음음
