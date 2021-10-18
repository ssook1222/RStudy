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
#문자형으로 하니 문제4 실행 후 문제가 발생하지 않았음.
lm_new$satisfaction<-ifelse(lm_new$satisfaction==9999,NA,lm_new$satisfaction) #9999가 값인 경우 NA로 처리

#결측치로 변경되었는지 확인
table(is.na(lm_new$payment))
table(is.na(lm_new$satisfaction))

# 문제 5. 문제4를 실행한 후, payment 변수에 발생한 문제를 해결하시오.
#바뀐 측정값을 원래 측정값으로 되돌리고 척도를 다시 변경

table(lm_new$payment)
# factor가 int 형으로 변경되어 이전 factor로 적용된 레벨로 적용된 문제가 발생
# 9999라는 숫자로 해줬기 때문인 것으로 보임.

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

lm_new%>% filter(expense>100000) #case 5개
lm_new<-lm_new%>%filter(expense<=100000)

#문제 8. 방문회수 대비 구매회수 비율을 나타내는 새로운 변수 rate를 만들고, 이 값이 NaN으로 나온 case에 대해 검토하시오.
lm_new <- lm_new%>% mutate(rate=order/visit)
summary(lm_new$rate)
#NaN은 결측치가 아니라 값을 모르는 것을 의미. (예: 0/0)
#아래는 예시
lm_new%>%filter(is.nan(rate))%>%select(rate,order,visit)

#문제 9. 가입경로에 따른 주문회수 평균, 결제수단에 따른 구매금액 평균, 성별에 따른 만족도 평균을 비교하시오.
lm_new%>%group_by(path)%>%summarise(mean(order))
as.data.frame(lm_new%>%group_by(payment)%>%summarise(mean(expense))) #변수(열)에 NA가 있는 거라 na.rm=T를 해도 사라지지 않음.
lm_new%>%group_by(gender)%>%summarise(mean(satisfaction,na.rm=T))

# 문제 10. 한 번이라도 주문한 고객들만으로 lm_order라는 데이터 프레임을 만들고 rate 변수를 삭제한 후, 
# 변수의 배열 순서를 case, ID, visit, duration, order, moring, expense, return,
# satisfaction, age, path, payment, gender, os로 변경하시오.
lm_order <- lm_new %>% filter(order>0)
lm_order$rate<-NULL
summary(lm_order)

lm_order$payment <- as.factor(lm_order$payment)
str(lm_order)
lm_order<-lm_order%>%relocate(os, .after=satisfaction)
lm_order<-lm_order%>%relocate(gender, .after=satisfaction)
lm_order<-lm_order%>%relocate(payment, .after=satisfaction)
lm_order<-lm_order%>%relocate(path, .after=satisfaction)

