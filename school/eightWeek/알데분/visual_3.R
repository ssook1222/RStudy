# 선 그래프 : 시계열 데이터 표현
library(dpylr)
library(ggplot2)
economics <- ggplot2::economics

str(economics) #date 제외하고는 다 실수라 척도 신경 쓸 필요 X

ggplot(economics,aes(date,unemploy))+geom_line() #전반적으로 실업자 수 증가

#점 추가, 특정 월 별 점 추가
ggplot(economics,aes(date,unemploy))+geom_line()+
  geom_point()

#색상 추가
ggplot(economics,aes(date,unemploy))+geom_line(color="red")+
  geom_point(color="darkred")

#boxPlot(상재그래프)
ggplot(mpg, aes(drv,highway,fill=drv))+geom_boxplot() #색상 다 다르게
ggplot(mpg, aes(drv,highway,fill=drv))+geom_boxplot(outlier.color="red")
ggplot(mpg, aes(drv,highway,fill=drv))+geom_boxplot(outlier.color="red")+
  stat_summary(fun="mean",geom="point") #평균을 점으로 표현

#corona19
corona19 <- read.csv("./school/data/corona19.csv", fileEncoding ="euc-kr",stringsAsFactors = F) 
str(corona19) #date가 문자

corona19$date <- as.Date(corona19$date)
summary(corona19)

#산점도
ggplot(corona19,aes(new_tests, new_cases))+geom_point()

#축 제한 + 추세선
ggplot(corona19,aes(new_tests, new_cases))+geom_point()+
  xlim(10000,60000)+ylim(0,3000)+geom_smooth()

ggplot(corona19,aes(date, new_cases, fill=new_cases))+geom_bar(stat="identity")
#확진자 수의 증감 추세를 막대 그래프로 보여짐

ggplot(corona19,aes(date, new_cases))+geom_line(color="red")+geom_point(color="blue")
ggplot(corona19,aes(date, new_deaths))+geom_line(color="red")+geom_point(color="blue")
ggplot(corona19,aes(date, total_deaths))+geom_line(color="red")+geom_point(color="blue")
ggplot(corona19,aes(date, positive.rate))+geom_line(color="red")+geom_point(color="blue")
#new_death와 비슷한 패턴
ggplot(corona19,aes(date, reproduction.rate))+geom_line(color="red")+geom_point(color="blue")
ggplot(corona19,aes(date, people_fully_vaccinated))+geom_line(color="red")+geom_point(color="blue")

#동시에 그리려면 scale을 맞춰줘야 함.
corona19_new <- read.csv("./school/data/corona19_new.csv", fileEncoding ="euc-kr",stringsAsFactors = F) 

corona19_new$date <- as.Date(corona19_new$date)
#type별로 색상 변경, 여러 개의 선을 그리므로 타입별로 색 구분
ggplot(corona19_new,aes(date,number,color=type))+geom_line()+geom_point()

