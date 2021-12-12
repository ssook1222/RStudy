#엑셀 파일 불러오기
library(readxl)

#문제 1

#데이터 프레임 생성
Logit <- read_excel("./school/data/Logit.xls",col_names = T, sheet = 1) #col_names :첫 행을 변수명으로 지정

#변수 척도 측정
str(Logit) #buy는 이진형으로 변경해야 됨
Logit$Buy <- as.factor(Logit$Buy)
#나머지는 실수형 척도가 정답

#문제 2
#로짓회귀식을 만들기 위해 glm함수가 필요

#로짓 회귀식 만들기 위해 glm 함수 불러오기
install.packages("mlogit")
library(mlogit)

#로짓회귀분석 식 보기
Logit_Buy1 <- glm(Buy~Visit+Age,
                  data = Logit,
                  family = binomial() #DV가 이진 분포임을 알림
                  )

summary(Logit_Buy1)

#문제3
#이상치 검토

library(car)

#케이스 중에서 이상치가 특이한 것 찾기
outlierTest(Logit_Buy1) 
#그나마 50번째 인덱스(데이터프레임에서 자동생성되는 인덱스)가 이상치일 가능성이 높음
#실제 이상치 확인은 Bonferroni p를 봐야 함. NA면 이상치가 아니라는 것임.
#Bonferroni p가 0.05보다 작은 값인 경우 이상치

#문제4
#모형적합도 확인

install.packages("ResourceSelection")
library(ResourceSelection)

hoslem.test(Logit_Buy1$y,Logit_Buy1$fit)
#hoslem.test(로짓회귀식$y, 로직회귀식$fit) 꼴임
#이 로짓회귀식의 p-value가 유의하지 않으므로 이 식은 유의함.

#문제 5
Logit_Buy2 <- glm(Buy~Visit+Age+Social,
                  data = Logit,
                  family = binomial()
)

summary(Logit_Buy2)

#문제 6
outlierTest(Logit_Buy2) #없음

#문제 7
hoslem.test(Logit_Buy2$y,Logit_Buy2$fit) #p value가 유의하지 않으므로 유의함

#문제 8
#카이제곱 검정으로 모형적합도 확인
#-2LL 값 감소가 유의한지, Social 추가한 게 적합한지 확인
#실제로는 12정도 떨어짐. 그런데 이 12가 통계적으로 유의한지 확인해야 함.


#순서 : 1. -2LL 변화 계산 2. 자유도 계산 3. 검증 진행

#순서 1. 2LL은 여기서 deviance
diff<- Logit_Buy1$deviance-Logit_Buy2$deviance

#순서 2. 자유도 차, df.residual이 자유도
dof <- Logit_Buy1$df.residual - Logit_Buy2$df.residual

#순서 3. p value에서 카이제곱 검정 구하기
pchisq(diff,dof) #앞에는 2LL 차, 뒤에는 자유도 차
#안쪽 넓이를 구한 것. 우리가 구하고자 하는 건 1에서 빼야 됨.

1-pchisq(diff,dof) #모형 적합도 증가, buy2의 설명력이 더 좋음


#문제 9

#순서1. 예측
#예측값 구하기
prediction1 <- predict(Logit_Buy1,newdata = Logit)

#순서2. 0과 1로 변경
prediction1 <- ifelse(prediction1<0,0,1)
# 그 후 요소로 변경
prediction1 <- as.factor(prediction1)

#순서3. 비교
#희소행렬로 비교하기 위해 caret 패키지 설치
install.packages("caret")
library(caret)

confusionMatrix(prediction1, Logit$Buy) #예측값과 실제값 서로 비교

prediction2 <- predict(Logit_Buy2,newdata = Logit)
prediction2 <- ifelse(prediction2<0,0,1)
prediction2 <- as.factor(prediction2)
confusionMatrix(prediction2, Logit$Buy)

#buy2의 정확도가 buy1의 정확도보다 높음


#문제 10. 새로운 사례 예측하기
#dataframe을 만들고 예측하기

c201 <- data.frame(Visit=5, Age=38, Social=11)
predict(Logit_Buy2,newdata = c201) #음수이므로 안 산다고 예측

#문제 11번 이전 lm_order 만들기
lm <- read.csv("./school/data/lm.csv", stringsAsFactors = F, fileEncoding = "EUC-KR")
lm_new <- lm

lm_new$path <- as.factor(lm_new$path)
lm_new$payment <- as.factor(lm_new$payment)
lm_new$gender <- as.factor(lm_new$gender)
lm_new$os <- as.factor(lm_new$os)

lm_new <- lm_new %>% filter(visit - order >= 0)
lm_new <- lm_new %>% filter(order - return >= 0)

library(tidyr)
lm_new <- lm_new %>% drop_na()

lm_new$payment <- ifelse(lm_new$payment == 9999, NA, lm_new$payment)
lm_new$satisfaction <- ifelse(lm_new$satisfaction == 9999, NA, lm_new$satisfaction)

lm_new$payment <- ifelse(lm_new$payment == 2, "간편결제", ifelse(lm_new$payment == 3, "계좌이체", "신용카드"))
lm_new$payment <- as.factor(lm_new$payment)

lm_new <- lm_new %>% filter(case != 177)
lm_new <- lm_new %>% filter(expense <= 100000) 
lm_order <- lm_new %>% filter(order > 0)

#문제 11
library(dplyr)
#mutate 이용해서 데이터프레임 업데이트
lm_order <- lm_order %>% mutate(return_YN = ifelse(lm_order$return==0,0,1))
lm_order$return_YN <- as.factor(lm_order$return_YN)

#문제 12
library(mlogit)
Logit_Return1 <- glm(return_YN~age+satisfaction,data=lm_order,family=binomial())
summary(Logit_Return1) #-2LL이 0에 가까울수록 모형에 적합한데, 890대라 정확하지 못함. 좋은 회귀식은 아님.

#문제 13
library(car)
outlierTest(Logit_Return1) #Bonferroni p가 NA이므로 이상치는 아님

#문제 14
library(ResourceSelection)
hoslem.test(Logit_Return1$y, Logit_Return1$fit) #p value가 유의하지 않으므로 모형 적합도는 맞음.
#그러나 좋은 회귀식이냐 하면 그건 아님.

#문제 15
Logit_Return2 <- glm(return_YN~age+satisfaction+expense,data=lm_order,family=binomial())
summary(Logit_Return2) #satisfaction의 경우 유의하지 않음

#문제 16
outlierTest(Logit_Return2) #없음

#문제 17
hoslem.test(Logit_Return2$y,Logit_Return2$fit) #p value가 유의하지 않으므로 모형 적합도는 유의함.

#문제 18
#순서 1  : 2LL / deviance
diff<- Logit_Return1$deviance-Logit_Return2$deviance
#순서 2 : dof / df.residual
dof <- Logit_Return1$df.residual-Logit_Return2$df.residual
#순서 3 : 카이제곱 검정 구하기
1-pchisq(diff,dof) #알파보다 유의하므로 return2 모델이 더 설명력이 좋음.

#문제 19
#예측력이 좋아야 좋은 모형. 설명력만 높다고 좋은 모델 아님. 
prediction3 <- predict(Logit_Return2,newdata = lm_order)
prediction3 <- ifelse(prediction3<0,0,1)
prediction3 <- as.factor(prediction3)
confusionMatrix(prediction3, lm_order$return_YN) 
#예측력이 78%, 80%가 안 되면 그렇게 좋은 모델이 아님.
#예측 결과 값이 0으로 쏠림. 예측한 결과 값이 1이 없음.
#분류를위한 지도학습

#문제 20
employment <- read.csv("./school/data/employment.csv",fileEncoding = "euc-kr")
str(employment) #DV는 범주형 척도로 변경해줘야 함.
employment$result <- as.factor(employment$result)
summary(employment)

employment$intern <- as.factor(employment$intern)

#문제 21
Logit_emp1 <- glm(result~GPA+test+ranking,data=employment,family=binomial())
summary(Logit_emp1) #모두 유의함.

#문제 22
outlierTest(Logit_emp1) #이상치가 없음

#문제 23
hoslem.test(Logit_emp1$y, Logit_emp1$fit) #유의하지 않으므로 모형 적합도가 적정

#문제 24
Logit_emp2 <- glm(result~GPA+test+self+intern,data=employment,family=binomial())
summary(Logit_emp2) #2p와 a 바로 비교

#문제 25
outlierTest(Logit_emp2) #이상치 없음

#문제 26
hoslem.test(Logit_emp2$y, Logit_emp2$fit) #a/2와 비교했을 떼 유의하지 않으므로 모형적합도가 맞음

#문제 27
#순서 1  : 2LL / deviance
diff<- Logit_emp1$deviance-Logit_emp2$deviance
#순서 2 : dof / df.residual
dof <- Logit_emp1$df.residual-Logit_emp2$df.residual
#순서 3 : 카이제곱 검정 구하기
1-pchisq(diff,dof) #알파보다 유의하므로 return2 모델이 더 설명력이 좋음.

#문제 28
prediction4 <- predict(Logit_emp2,newdata = employment)
prediction4 <- ifelse(prediction4<0,0,1)
prediction4 <- as.factor(prediction4)
confusionMatrix(prediction4, employment$result) #0.822

#문제 29
c501 <- data.frame(GPA=3.58,test=110,self=9,intern=1)
c501$intern <- as.factor(c501$intern)

prediction5 <- predict(Logit_emp2,newdata = c501)
prediction5 <- ifelse(prediction5<0,0,1)
prediction5 #불합격

  






