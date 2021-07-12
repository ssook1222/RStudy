#dplyr이 데이터 전처리에 많이 사용

install.packages("dplyr")
library(dplyr)

exam<-read.csv("./Day3/csv_exam.csv")
exam

#math 평균 산출
exam %>% summarise(mean_math=mean(math))
exam %>%
  group_by(class)%>% #class 별로 분리
  summarise(mean_math=mean(math)) #math 평균 산출

exam %>%
  group_by(class)%>%
  summarise(mean_math=mean(math),
            sum_math=sum(math),
            median_math=median(math), n=n())

#mean(): 평균, sd():표준편차, sum():합계, median(): 중앙값, min(): 최솟값, max(): 최댓값, n():빈도

#mpg 데이터 사용
install.packages('dplyr')
library(dplyr)

mpg %>%
  group_by(manufacturer,drv)%>%
  summarise(mean_cty=mean(cty))%>%
  head(10)

#회사별로 suv 자동차의 도시 및 고속도로 통합 연비 평균을 구해 내림차순으로 정렬하고, 1~5위까지 출력하기
mpg%>%
  group_by(manufacturer)%>%
  filter(class=="suv")%>%
  mutate(tot=(cty+hwy)/2)%>%
  summarise(mean_tot=mean(tot))%>%
  arrange(desc(mean_tot))%>%
  head(5)
