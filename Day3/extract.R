#dplyr이 데이터 전처리에 많이 사용

install.packages("dplyr")
library(dplyr)

exam<-read.csv("./Day3/csv_exam.csv")
exam

#변수 추출하기
exam %>% select(math)

#여러 변수 추출하기
exam %>% select(class,math, english)

#변수 제외하기
exam %>% select(-math)

#filter와 select 조합하기
exam %>% filter(class==1) %>% 
         select(english)

#일부만 출력하기
exam %>%
  select(id, math) %>%
  head
