#복습
mpg <- ggplot2::mpg
mpg
#배기량에 따른 도시연비 비교
library(ggplot2)
ggplot(mpg, aes(x = displ, y = hwy)) + geom_point() + geom_smooth() + labs(title = "<배기량에 따른 도시연비 비교>") 

ggplot(data=economics,aes(x=date,y=umemploy))+geom_line()+theme_economist() 

#1. 제조사가 audi인 행만 뽑기 : baseR사용.
#2. displ이 2보다 작은 것
mpg[mpg$manufacturer == "audi", ]
mpg[mpg$displ<2,]

library(foreign)
library(dplyr)
setwd("C:/Rdata")
welfare <- read.spss("koweps_hpc10_2015_beta1.sav", to.data.frame = T)

welfare <- rename(welfare,
                  gender=h10_g3,
                  birth=h10_g4,
                  marriage=h10_g10,
                  religion=h10_g11,
                  income=p1002_8aq1,
                  code_job=h10_eco9,
                  code_region=h10_reg7)
#1.  자료를 이용하여 남자 평균 소득과 여자 평균 소득 구하기

welf1 <- welfare %>% 
  group_by(gender) %>% 
  summarise(n=mean(income, na.rm=T))


#2. 1번에서 구해진 값을 이용하여 그래프 표현하기
ggplot(aes(x=gender, y=avg, fill=gender))+
  geom_col() 

#welfare$gender를 1은 'male', 2는 'female'로
welfare$gender <- ifelse(welfare$gender == 1, 'male', 'female')

#1. R에서 말하는 factor는 어떤 의미인가
범주

#2. dataframe을 tibble형태로 바꾸려고 할 때 쓰는 명령어는?
as_tibble()

read.csv()
read_excel()
readExcel
read_excel

#칼럼 marriage에는 0부터 6까지의 값이 들어있다.
# 1은 "기혼", 나머지는 "미혼"으로 바꾸시오.
welfare$marriage
welfare$marriage <- ifelse(welfare$marriage==1, "기혼", "미혼")
mode(welfare$marriage)

#20대의 혼인상태를 막대그래프로 보이시오.
welfare$age <- 2015-welfare$birth
welfare %>% 
  filter(age<30&age>=20) %>% 
  ggplot(aes(x=age,marriage))+
  geom_bar(position="dodge") 
  
#수업
#연령대별로 쪼개기
welfare <- welfare %>% 
  mutate(age_gen = ifelse(age<30, "young",
                          ifelse(age<=50, "middle", "old")))
head(welfare)
dim(welfare)


age_gen_income <- welfare %>% 
  group_by(age_gen) %>% 
  summarise(mean_income = mean(income, na.rm=T))
ggplot(age_gen_income, aes(age_gen, mean_income, fill=age_gen)) + geom_col()
welfare1

welfare %>%
  group_by(age_gen) %>% 
  summarise(mean_income = mean(income, na.rm = T)) %>% 
  ggplot(aes(x=age_gen, y=mean_income, fill=age_gen)) +
  geom_col() 

ggplot(data=age_gen_income, aes(x=age_gen, y=mean_income)) +
  geom_col(aes(fill=age_gen)) +
  scale_x_discrete(limits = c("young", "middle", "old"))


welfare$age <- 2015-welfare$birth

welfare <- welfare %>% 
  mutate(age_gen = ifelse(age<30, "young",
                          ifelse(age<=50, "middle", "old")))
welfare %>% group_by(age_gen,sex) %>% 
  summarise(mean_income=mean(income,na.rm=T)) %>% 
  ggplot(aes(x=age_gen,y=mean_income,fill=sex))+
  geom_col(position="dodge") +
  scale_x_discrete(limits=c("young", "middle", "old")) 


welfare$age <- 2015-welfare$birth
welfare <- welfare %>% 
  mutate(age_gen = ifelse(age<30, "young",
                          ifelse(age<=50, "middle", "old")))
gender_income <- welfare %>% 
  group_by(age_gen, gender) %>% 
  summarise(mean_income = mean(income, na.rm=T))


library(readxl)
setwd("c:/Rdata")
list_job <- read_excel("Koweps_Codebook.xlsx", col_names=T, sheet=2)
welfare <-left_join(welfare, list_job, by="code_job")
list_job$code_job

list_job$job

names(list_job) <- c("job", "직업")
list_job
welfare[1:6, c("job", 'income')]

welfare <- left_join(welfare, list_job, by="job")
head(welfare)
#직업별 평균소득
top10 <- welfare %>% 
  group_by(직업) %>% 
  summarise(평균소득 = mean(income, na.rm=T)) %>% 
  arrange(desc(평균소득)) %>% head(10)

ggplot(data=top10, aes(x=reorder(직업, 평균소득), y= 평균소득, 직업)) + geom_col()
+coord_flip()

#지역별 인구 분포 분석하기
welfare <- read.spss("koweps_hpc10_2015_beta1.sav", to.data.frame = T)
welfare <- welfare %>%  select(gender=h10_g3,
                               birth=h10_g4,
                               marriage=h10_g10,
                               religion=h10_g11,
                               income=p1002_8aq1,
                               job=h10_eco9,
                               region=h10_reg7)

welfare <- left_join(welfare, region_map, by="region")

region_map <- data.frame(region = c(1,2,3,4,5,6,7),
                         지역 = c("서울", "수도권(인천/경기)", "부산/경남/울산", "대구/경북", "대전/충남", "강원/충북", "광주/전남/전북/제주도"))
head(welfare)
welfare %>% group_by(지역) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x=지역, y=n, fill=지역))+
  geom_col()+
  labs(x="지역명",y="인구수") 
round(11/3, 2)

#10-1
#KoNLP
#JAVA

#https://mrchypark.github.io/post/KoNLP-%EC%84%A4%EC%B9%98-%EB%B0%A9%EB%B2%95/

##의존성 패키지 설치
install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")

## 깃허브 통해 KoNLP 다운로드
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))   
#또는
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", force = TRUE, INSTALL_opts=c("--no-multiarch"))

#java 다운받고 C:/Users/user/AppData/Local/R/win-library/4.2/KoNLP/java에 압축풀기
#https://drive.gumu.kr/sharing/9etX101Gs
library(KoNLP)

useNIADic()
useSejongDic()

text <- "안녕하세요. 좋은 아침입니다."
extractNoun(text)

install.packages(".../path/to/package.tar.gz", type="source", repos=NULL)

setwd("C:/RData")
text <- readLines("ahn.txt")
nouns <- extractNoun(text)

words <- unlist(nouns)
words <- words[nchar(words) >= 2]
df <- as.data.frame(table(words))
head(df)
df <- df %>% arrange(desc(Freq)) %>% head(10)
# wordcloud2 install하기
wordcloud2(df)
coord_flip()

df
ggplot(df, aes(x=reorder(words,Freq),y=Freq, fill=words)) + 
  geom_col() + coord_flip()

#인터랙티브
#plotly설치치

ggplot(mpg, aes(x=displ, y=hwy, color=drv)) + geom_point()
ggplotly(ggplot(mpg, aes(x=displ, y=hwy, color=drv)) + geom_point()
)

libarar(xts)
getwd()
