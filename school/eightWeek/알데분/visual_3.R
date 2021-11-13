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
Logit <- read_csv("./school/data/Logit.xls", ) 


