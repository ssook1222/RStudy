#dplyr이 데이터 전처리에 많이 사용

install.packages("dplyr")
library(dplyr)

exam<-read.csv("./Day3/csv_exam.csv")
exam

# class가 1인 경우만 출력 (특정 열만 출력)
exam %>% filter(class==1)

# 부등호로 조건 걸기
exam %>% filter(math>50)

#조건이 여러개인 경우
exam %>% filter(math>=90 | english >=90)

# 추출한 행으로 데이터 만들기
class1 <- exam%>% filter(class==1)
mean(class1$math)
