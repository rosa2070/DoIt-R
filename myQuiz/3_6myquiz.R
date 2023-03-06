#인구가 가장 많은 종교 5개별 성별 비교
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

raw_welfare <- read.spss(file="Koweps_hpc10_2015_beta1.sav",
                         to.data.frame=T)
welfare <- raw_welfare
welfare <- rename(welfare,
                  sex=h10_g3,
                  religion=h10_g11)

#성별 변수 전처리
table(welfare$sex)
welfare$sex <- ifelse(welfare$sex == 9, NA, welfare$sex)
table(is.na(welfare$sex))
welfare$sex <- ifelse(welfare$sex==1, "male", "female")
table(welfare$sex)


#종교 변수 전처리
table(welfare$religion)
welfare$religion <- ifelse(welfare$religion == 9, NA, welfare$religion)
table(is.na(welfare$religion))
welfare$religion <- ifelse(welfare$religion==1, "yes", "no")

table(welfare$religion)

#성별에 따른 종교유무 
sex_Religion <- welfare %>% 
  group_by(religion, sex) %>% 
  summarise(n=n())
sex_Religion

ggplot(data=sex_Religion, aes(x=sex, y=n, fill=religion)) + geom_col(position = "dodge") 


