library(rvest)
library(stringr)
library(dplyr)
#변하지 않는 변수 for문 바깥에 저장장
title = c()
body = c()

url_1 <- "https://search.daum.net/search?w=news&nil_search=btn&DA=NTB&enc=utf8&cluster=y&cluster_page=1&q=%EB%89%B4%EC%A7%84%EC%8A%A4&p="


for(i in 1:10){
  #주소를 쪼개서 합쳐
  url <- paste(url_1, i, sep = "")
  print(url)
  
  #웹페이지 주소로 달려가
  hdoc <- read_html(url)
  
  #클래스 이름 잡기기 -> 클래스에서 글자만 뽑아내기
  title_part <- hdoc %>% html_nodes(".tit_main.fn_tit_u") %>% html_text()
  body_part <- hdoc %>% html_nodes(".desc") %>% html_text()
  
  title <- c(title, title_part)
  body <- c(body, body_part)
}

#news = data.frame(제목 = title, 본문 = body)

news = cbind(title, body)


write.csv(news, "뉴진스.csv") 

#wordcloud를 만들어봅시다.빈도수 상위 20개를 wordcloud로 만들어봅시다.
library(KoNLP)

useNIADic()
useSejongDic()


newjeans <- read.csv("뉴진스.csv", header=T, fileEncoding="UTF-8")
newjeans <- str_replace_all(newjeans, "\\W", " ")
nouns <- extractNoun(newjeans)
wordcount <- table(unlist(nouns))
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
df_word <- rename(df_word, 
                  word=Var1,
                  freq= Freq)
df_word <- filter(df_word, nchar(word)>=2)
top_20 <- df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)
top_20

install.packages("wordcloud2")
library(wordcloud2)
library(RColorBrewer)

pal <- brewer.pal(8, "Dark2")
set.seed(1234)
wordcloud2(top_20)

x <- 1:12
# a random permutation
sample(x)
x <- 1:5
sample(x,3)
sample(x,3,replace = TRUE)
# bootstrap resampling -- only if length(x) >

set.seed(1234)
sample(1:10, 5)


exam <- read.csv("csv_exam.csv")
exam <- exam[,2:ncol(exam)]
exam <- exam[ , -1]
exam[seq(1,20,2), ]
dim(exam)[2]
length(names(exam))

exam[1:10, ]       
exam[seq(1,20,2),]

set.seed(1234)
exam[sample(1:20, 10), ]

#5394개를 뽑습니다.
library(ggplot2)

diamonds[sample(1:53940, 5394), ]
num <- sample(nrow(diamonds), nrow(diamonds)*.1)
dia <- diamonds[num,]

dia_2 <- diamonds[-num,]

#diamonds를 5: 3: 2비율로 3개의 서로 다른 세트를 만들어봅시다.

ggplot(dia, aes(x=carat, y=price, color=color)) + geom_point()

num <- sample(nrow(diamonds), nrow(diamonds)*.5)
num1 <- sample(nrow(temp), nrow(diamonds)*.3)

train <- diamonds[num,]
temp <- diamonds[-num,]
val <- temp[num1,]
test <- temp[-num1,]

nrow(train)
nrow(val)
nrow(test)

rm(diamonds)
sample_n(diamonds, nrow(diamonds)*0.5)
#fraction = 분수,비율
sample_frac(diamonds, 0.5)

sample(c("A", "B"), 10, replace = T, prob = c(5,2))


ggplot(mpg, aes(x=drv, fill= drv)) + 
  geom_bar() + 
  scale_fill_hue(c = 100)

ggplot(mpg, aes(x=drv, fill=drv)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set5")


install.packages("tibble")
library(tibble)
library(ggplot2)
USArrests %>% head

crime <- rownames_to_column(USArrests, var = "state")
crime$state <- tolower(crime$state)


states_map <- map_data(("state"))
states_map <- as_tibble(states_map)

choropleth : 단계구분도

ggChoropleth(data = crime, aes(fill=Murder, 
                               map_id = state), 
             map = states_map)







