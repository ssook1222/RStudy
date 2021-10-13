#문제1: 주(state)별로 미성년인구백분율 평균을 구했을 때, 평균이 가장 낮은 지역과 평균을 각각 구하시오. 

#조건에 맞게 오름차순으로 추출한 전체 테이블 확인
midwest%>%
  group_by(state)%>%
  summarise(percyouth_mean=mean(percyouth,na.rm = T))%>%
  arrange(percyouth_mean)

#가장 낮은 지역
(midwest%>%
  group_by(state)%>%
  summarise(percyouth_mean=mean(percyouth,na.rm = T))%>%
  arrange(percyouth_mean)%>%
    head(1))$state
#IL

#가장 높은 지역
(midwest%>%
    group_by(state)%>%
    summarise(percyouth_mean=mean(percyouth,na.rm = T))%>%
    arrange(percyouth_mean)%>%
    tail(1))$state
#OH

#문제2: 주(state)별로 아시아인구백분율 평균을 구했을 때, 평균이 가장 높은 지역과 평균을 각각 구하시오.
midwest%>%
  group_by(state)%>%
  summarise(percasian_mean=mean(percasian,na.rm = T))%>%
  arrange(percasian_mean)

#가장 낮은 지역
(midwest%>%
    group_by(state)%>%
    summarise(percasian_mean=mean(percasian,na.rm = T))%>%
    arrange(percasian_mean)%>%
    head(1))$state
#IN

#가장 높은 지역
(midwest%>%
    group_by(state)%>%
    summarise(percasian_mean=mean(percasian,na.rm = T))%>%
    arrange(percasian_mean)%>%
    tail(1))$state
#IL

#문제3: 아래 표를 참고하여 새로운 변수 grade를 만드시오.

#PID 병합 버전
library(dplyr)
midwest_check<-midwest_check%>%mutate(percseni=((senior/poptotal)*100))

midwest_check<-midwest_check%>%mutate(grade=ifelse(percseni>=15,"very large",ifelse
                                       (percseni>=8&percseni<15,"large",
                                         ifelse(percseni>=5&percseni<8,
                                           "middle","small"))))



#기존 버전(county와 region으로 병합)
midwest_test<-midwest_test%>%mutate(percseni=((senior/poptotal)*100))

midwest_test<-midwest_test%>%mutate(grade=ifelse(percseni>=15,"very large",ifelse
                                                   (percseni>=8&percseni<15,"large",
                                                     ifelse(percseni>=5&percseni<8,
                                                            "middle","small"))))

#문제4: grade에 대한 빈도를 구하시오. 
#PID 병합 버전
table(midwest_check$grade)

#기존 버전
table(midwest_test$grade)

#문제5: midwest_income.csv에는 county별로 1인당 연간 소득(단위: 달러) 변수 income을 포함하고 있다. 
#동일한 이름의 데이터 프레임을 만들고 midwest와 midwest_income을 통합한 후, 
#income 전체 평균을 구하시오. 

midwest_income<-read.csv("./school/data/midwest_income.csv",fileEncoding ="euc-kr",stringsAsFactors = F)
midwest_new<-left_join(midwest,midwest_income,by=c("PID"="PID"))
mean(midwest_new$income, na.rm = T)

#67266.05

#문제6: 미성년인구비율이 40%이상이고, senior 비율이 5% 미만인 county의 연간소득 평균값은 얼마인가?
#해본 결과, PID 병합이나 county-region 병합이나 결과 동일
midwest_test<-left_join(midwest_test,midwest_income,by=c("PID"="PID"))
midwest_check<-left_join(midwest_check,midwest_income,by=c("PID"="PID"))

# midwest_test%>%filter(percyouth>=40&percseni<5)%>%group_by(county)%>%summarise(mean_income=mean(income))
midwest_test%>%filter(percyouth>=40&percseni<5)%>%select(county,income)%>%summarise(mean_income=mean(income))
midwest_check%>%filter(percyouth>=40&percseni<5)%>%select(county,income)%>%summarise(mean_income=mean(income))
#73610.13



