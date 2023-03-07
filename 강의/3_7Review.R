#9-1
#데이터분석 준비하기
install.packages("foreign")

library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

raw_welfare <- read.spss(file = "Koweps_hpc10_2015_beta1.sav",
                         to.data.frame = T)
welfare <- raw_welfare

welfare <- rename(welfare,
                  sex = h10_g3,
                  birth = h10_g4,
                  marriage = h10_g10,
                  religion = h10_g11,
                  income = p1002_8aq1,
                  code_job = h10_eco9,
                  code_region = h10_reg7)
#9-2
#성별 변수 검토 및 전처리
class(welfare$sex)
table(welfare$sex)

table(welfare$sex)

welfare$sex <- ifelse(welfare$sex == 9, NA, welfare$sex)
table(is.na(welfare$sex))

welfare$sex <- ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)
qplot(welfare$sex)

#월급 변수 검토 및 전처리
class(welfare$income)
summary(welfare$income)
qplot(welfare$income) + xlim(0,1000)

summary(welfare$income)

welfare$income <- ifelse(welfare$income %in% c(0,9999), NA, welfare$income)
table(is.na(welfare$income))

#성별에 따른 월급 차이 분석하기
sex_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income = mean(income))

sex_income
ggplot(sex_income, aes(sex, mean_income)) + geom_col()

#9-3
#나이 변수 검토 및 전처리
class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)
summary(welfare$birth)
table(is.na(welfare$birth))

welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth)
table(is.na(welfare$birth))

welfare$age <- 2015 - welfare$birth + 1
summary(welfare$age)
qplot(welfare$age)

#나이와 월급의 관계 분석하기
age_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income))

head(age_income)
ggplot(age_income, aes(age, mean_income)) + geom_line()

#9-4
#연령대 변수 검토 및 전처리하기
welfare <- welfare %>% 
  mutate(ageg = ifelse(age<30, "young", 
                       ifelse(age<=59, "middle", "old")))
table(welfare$ageg)
qplot(welfare$ageg)

#연령대에 따른 월급 차이 분석하기
ageg_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean_income = mean(income))
ageg_income

ggplot(ageg_income, aes(ageg, mean_income)) + 
  geom_col() +
  scale_x_discrete(limits = c("young", "middle", "old"))

#9-5
#연령대 및 성별 월급 차이 분석하기
sex_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg, sex) %>% 
  summarise(mean_income = mean(income))
sex_income

ggplot(sex_income, aes(ageg, mean_income, fill=sex)) + 
  geom_col(position= "dodge") + 
  scale_x_discrete(limits = c("young", "middle", "old")) 

#나이 및 성별 월급 차이 분석하기
sex_age <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age, sex) %>% 
  summarise(mean_income = mean(income))

head(sex_age)
ggplot(sex_age, aes(age, mean_income, col=sex)) + geom_line()

#9-6
#직업 변수 검토 및 전처리하기

class(welfare$code_job)
table(welfare$code_job)

library(readxl)
list_job <- read_excel("Koweps_Codebook.xlsx", col_names = T, sheet=2)
head(list_job)
dim(list_job)

welfare <- left_join(welfare, list_job, by="code_job")

welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job, job) %>% 
  head(10)

#직업별 월급 차이 분석하기
job_income <- welfare %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income))

head(job_income)

top10 <- job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)

top10
ggplot(top10, aes(x=reorder(job, mean_income), y=mean_income)) + 
  geom_col() +
  coord_flip()

bottom10 <- job_income %>% 
  arrange(mean_income) %>% 
  head(10)

bottom10
ggplot(bottom10, aes(x=reorder(job, -mean_income), y=mean_income)) + 
  geom_col() +
  coord_flip() +
  ylim(0,850)

#9-7
#성별 직업 빈도 분석하기
job_male <- welfare %>% 
  filter(!is.na(job) & sex =="male") %>% 
  group_by(job) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  head(10)
job_male

job_female <- welfare %>% 
  filter(!is.na(job) & sex =="female") %>% 
  group_by(job) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  head(10)
job_female

ggplot(job_male, aes(x=reorder(job, n), y=n)) +
  geom_col() +
  coord_flip()

ggplot(job_female, aes(x=reorder(job, n), y=n)) +
  geom_col() +
  coord_flip()

#9-8
#종교 변수 검토 및 전처리하기
class(welfare$religion)
table(welfare$religion)

welfare$religion <- ifelse(welfare$religion==1, "yes", "no")
table(welfare$religion)
qplot(welfare$religion)

#혼인 상태 변수 검토 및 전처리하기
class(welfare$marriage)
table(welfare$marriage)

welfare$group_marriage <- ifelse(welfare$marriage==1, "marriage",
                                 ifelse(welfare$marriage==3, "divorce", NA))
table(welfare$group_marriage)
table(is.na(welfare$group_marriage))
qplot(welfare$group_marriage)

#종교 유무에 따른 이혼율 분석하기
religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(religion, group_marriage) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group = sum(n),
         pct = round(n/tot_group*100, 1))
religion_marriage

religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  count(religion, group_marriage) %>% 
  group_by(religion) %>% 
  mutate(pct = round(n/sum(n)*100, 1))

divorce <- religion_marriage %>% 
  filter(group_marriage=="divorce") %>% 
  select(religion, pct)

divorce

ggplot(divorce, aes(religion, pct)) + geom_col()

#연령대 및 종교 유무에 따른 이혼율 분석하기
ageg_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(ageg, group_marriage) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group=sum(n),
         pct=round(n/tot_group*100, 1))
ageg_marriage  

ageg_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  count(ageg, group_marriage) %>% 
  group_by(ageg) %>% 
  mutate(pct = round(n/sum(n)*100,1))

  
