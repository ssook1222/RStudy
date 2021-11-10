library(dplyr)
library(car) #산점도
library(psych) #상관관계
library(QuantPsyc) #표준화 회귀계수

#사전 준비
bike_MDA<-read.csv("./school/data/bike_MDA.csv", fileEncoding ="euc-kr", stringsAsFactors = F)
bike_MDA$time <- as.POSIXct(bike_MDA$time)
bike_MDA <- bike_MDA%>%mutate(difference=registered-casual)
library(tidyr)
bike_MDA<-bike_MDA%>%drop_na()                            

# case, season, holiday, working, weather
bike_MDA$case <- as.factor(bike_MDA$case)
bike_MDA$season <- as.factor(bike_MDA$season)
bike_MDA$holiday <- as.factor(bike_MDA$holiday)
bike_MDA$working <- as.factor(bike_MDA$working)
bike_MDA$weather <- as.factor(bike_MDA$weather)

# total 변수의 측정값이 650회 이상이면 outlier로 판정하였다. 이처럼 outlier로 판정된 
# 296개 case를 제거함으로써 bike_MDA 데이터 프레임을 업데이트 하시오,

table(bike_MDA$total>=650)
bike_MDA$total<-ifelse(bike_MDA$total>=650,NA,bike_MDA$total)
bike_MDA<-bike_MDA%>%drop_na()   

#문제1
str(bike_MDA)
corr.test(bike_MDA[,c(7:14)], 
          method="pearson",
          alpha = 0.05,
          use="pairwise.complete.obs")

#total

#문제2
#+/+/-/+/+


#문제3
lm_bike1<-lm(total~temp+atemp+humidity+windspeed+difference,data=bike_MDA)
plot(lm_bike1)

#인덱스 6942,6943,6945(여기서는 인덱스, 케이스 동일)

#문제 4
ks.test(bike_MDA$total, pnorm, mean=mean(bike_MDA$total),
        sd=sd(bike_MDA$total))

# #도출된 pvalue는 사실상 2p-value이므로 2p-value를 유의계수와 비교
#**유의하지 않아야 정규성 조건 만족

#답: 검증결과 p-value값이 2.2e-16보다 작게 나와, 여기에 2를 곱해도 2α인 0.1보다 작은 값이므로 유의하게 나온다. 이는 검증 결과의 p value가 유의하지 않아야 정규성 조건을 만족하므로 곧 정규성 조건에 위배된다는 것을 의미한다.

#문제 5
durbinWatsonTest(lm_bike1)
#답: 2번, 양의 자기상관


#문제 6
summary(lm_bike1)
# 답: p-value가 0.36345로 α에 2를 곱한 값인 0.1보다 크게 나왔기 때문에 해당 가설은 채택 불가능하다.

#문제 7
library(car)
vif(lm_bike1)

#답 1, atemp

#문제 8
lm_bike2<-lm(total~temp+humidity+windspeed+difference,data=bike_MDA)
vif(lm_bike2)
summary(lm_bike2)

#답: 0.7539


#문제 9
lm.beta(lm_bike2)

#답: humidity 

#문제 10
lm_bike3<-lm(total~temp+humidity+windspeed+difference+holiday,data=bike_MDA)
summary(lm_bike2)
summary(lm_bike3)

#답: 0.001

#문제 11
anova(lm_bike2,lm_bike3)
#문제 10번 결과 R^2이 증가하였고,
#f통계로 확인해본 결과 p-value가 1.31e-11로 0.05보다 작으므로 holiday 변수 추가는 유의미한 추가로 모형 개선에 도움이 됨.

#문제 12
contrasts(bike_MDA$holiday)
summary(lm_bike3)
# 공휴일일 때(1일때)가 아닐 때보다 30.35756배 만큼 더 total(DV)에 영향을 미친다.

#문제 13
lm_bike_s1 <- lm(total~temp+humidity+windspeed+difference,
                     data=bike_MDA,
                     subset=(season %in% c(1,2)))

lm_bike_s2 <- lm(total~temp+humidity+windspeed+difference,
                 data=bike_MDA,
                 subset=(season %in% c(3,4)))

summary(lm_bike_s1)
summary(lm_bike_s2)

#답: windspeed 
