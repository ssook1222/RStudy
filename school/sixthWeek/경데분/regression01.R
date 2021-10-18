library(dplyr)
lm<-read.csv("./school/data/lm.csv", fileEncoding ="euc-kr", stringsAsFactors = F)
lm_new<-lm

#문제 1. chr를 factor로 바꾸시오.
str(lm_new)
lm_new$path<-as.factor(lm_new$path)
lm_new$payment<-as.factor(lm_new$payment)
lm_new$gender<-as.factor(lm_new$gender)
lm_new$os<-as.factor(lm_new$os)

#문제 2. visit>=order, order>=moring, order>=return 관계를 위배하는 case를 식별하여 제거하시오.
table(lm_new$visit-lm_new$order<0) #위배하는 케이스 개수 확인
lm_new%>%filter(visit-order<0) #위배하는 케이스 6개 필터로 출력
lm_new%>%filter(visit-order<0)%>%select(case,visit,order) #위배하는 케이스 6개 중 case, visit, order 3개의 변수 출력

table(lm_new$order-lm_new$morning<0) #위배하는 케이스 개수 확인(위배하는 케이스 개수 없음)
table(lm_new$order-lm_new$return<0) #위배하는 케이스 개수 3개
lm_new %>% filter(order-return<0)%>%select(case,order,return)

#위배한 케이스 제거
lm_new<-lm_new%>%filter(visit-order>=0) 
lm_new<-lm_new%>%filter(order-return>=0) 

#문제 3. NA(Raw Data가 공란)이 존재하는지 검토하고, 많지 않다면 NA를 하나라도 포함한 case는 제거하시오.
summary(lm_new) #NA 개수 확인
library(tidyr)
lm_new<-lm_new%>%drop_na() #NA 제거

# 문제 4. 9999로 표기된 측정 값을 NA로 변경하시오.
table(lm_new$expense==9999) #9999로 측정된 값 개수 확인 (0개)
table(lm_new$payment==9999) #9999로 측정된 값 개수 확인 (155개)
table(lm_new$satisfaction==9999) #9999로 측정된 값 개수 확인 (155개)

lm_new$payment<-ifelse(lm_new$payment==9999,NA,lm_new$payment) #9999가 값인 경우 NA로 처리
lm_new$satisfaction<-ifelse(lm_new$satisfaction==9999,NA,lm_new$payment) #9999가 값인 경우 NA로 처리

#결측치로 변경되었는지 확인
table(is.na(lm_new$payment))
table(is.na(lm_new$satisfaction))

# 문제 5. 문제4를 실행한 후, payment 변수에 발생한 문제를 해결하시오.
#바뀐 측정값을 원래 측정값으로 되돌리고 척도를 다시 변경

table(lm_new$payment)
# factor가 int 형으로 변경되며 이전 factor로 적용된 숫자가 적용된 문제가 발생

# 바뀐 측정값을 원래 측정값으로 되돌림
lm_new$payment<-ifelse(lm_new$payment==2, "간편결제", ifelse(lm_new$payment==3,"계좌이체","신용카드"))

# 문제 6. order가 0이면 expense가 0이 되어야 하고 payment와 satisfaction은 NA가 되어야 하는데 이 조건을 위배하는 
# case를 식별한 후 제거하시오.
lm_new%>%filter(order==0)%>%
  select(case,order,expense,payment,satisfaction) #order가 0인 케이스 중 case, order, expense, payment, satisfaction 출력

lm_new_order<-lm_new%>%filter(order==0)%>%
  select(case,order,expense,payment,satisfaction) # order가 0인 케이스 새 데이터 프레임에 할당

lm_new_order%>%filter(expense!=0) #expense가 0이 아닌 케이스  (0개)
lm_new_order%>%filter(!is.na(payment)) #payment가 NA가 아닌 케이스 (1개)
lm_new_order%>%filter(!is.na(satisfaction)) #payment가 NA가 아닌 케이스 (1개)
lm_new<-lm_new%>%filter(case!=177)

rm(lm_new_order) #케이스 찾고 테이블 삭제

# 문제 7. order, return, expense에 대해 boxplot를 이용하여 outlier를 확인하시오.
# expense가 1억 원을 초과하는 outlier에 해당하는 case를 제거하시오.
library(ggplot2)

#boxplot 그리기
ggplot(lm_new,aes(1,order))+geom_boxplot()

#이상치에 색상 부여한 후 평균 표현하는 점 추가
ggplot(lm_new,aes(1,order))+geom_boxplot()+geom_boxplot(outlier.color = "red")+
  stat_summary(fun="mean", geom="point")

ggplot(lm_new,aes(1,return))+geom_boxplot()+geom_boxplot(outlier.color = "red")+
  stat_summary(fun="mean", geom="point")

ggplot(lm_new,aes(1,expense))+geom_boxplot()+geom_boxplot(outlier.color = "red")+
  stat_summary(fun="mean", geom="point")

lm_new%>% filter(expense>100000)






