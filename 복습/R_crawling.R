#크롤링
#https://brunch.co.kr/@dugi/12

library(rvest)
library(stringr)
library(dplyr)

title = c()
body = c()

url_1 <- "https://search.daum.net/search?w=news&DA=PGD&enc=utf8&cluster=y&cluster_page=1&q=%EB%A5%B4%EC%84%B8%EB%9D%BC%ED%95%8C&p="

for(i in 1:30) {
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
df <- df %>%  arrange(desc(Freq)) %>% head(10)

library(wordcloud2)
wordcloud2(df)
