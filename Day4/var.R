#dplyr이 데이터 전처리에 많이 사용

install.packages("dplyr")
library(dplyr)

exam<-read.csv("./Day3/csv_exam.csv")
exam

#math 오름차순 정렬
exam %>% arrange(math)

#math 내림차순 정렬
exam %>% arrange(desc(math))

#class 및 math 오름차순 정렬
exam %>% arrange(class,math)

#파생변수 하나 추가하기
exam %>%
  mutate(total = math+english+science) %>%
  head

# 파생변수 여러 개 추가하기
exam %>%
  mutate(total=math+english+science, mean=(math+english+science)/3) %>%
head

# mutate에 ifelse 적용
exam %>%
  mutate(test=ifelse(science>=60, "pass","fail")) %>%
head

#총합변수 추가
exam %>%
  mutate(total=math+english+science)%>%
  arrange(total)%>%
  head
