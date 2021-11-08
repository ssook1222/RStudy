#문제1 데이터 프레임 만들고 변수 척도 구분
library(readxl)
Logit <- read_excel("./school/data/Logit.xls", col_names = T, sheet = 1) 
#col_names : 변수명은 그대로 냅두기
str(Logit) #모든 변수가 numeric임, 그러나 DV인 buy는 범주형으로 변경
Logit$Buy <- as.factor(Logit$Buy)

#문제2 Visit과  Age만 IV로 고려한 Logit_Buy1을 추정하시오.
install.packages("mlogit")
library(mlogit) #logit회귀분석 만드는 패키지 

Logit_Buy1 <- glm(Buy~Visit+Age,data = Logit,
                  family = binomial() #종속변수가 이항분포를 그린다고 지정
                  )
summary(Logit_Buy1)

#문제 3 이상치 테스트
library(car)
outlierTest(Logit_Buy1) #측정값이 outlier인 것을 파악, 측정값이 특이한 케이스를 찾음

#50번째 인덱스의 데이터가 outlier일 가능성이 높음
#실제 outlier에 판정하려면 Bonferroni p를 봐야 함. NA면 이상치가 아님
#Bonferroni P값이 유의하다고 나오면 해당 모델이 이상치로 봄 -> 주관적 판단 필요

#문제4 모형 적합도 확인
install.packages("ResourceSelection")
library(ResourceSelection) #모형 적합도 확인 함수가 담긴 패키지

hoslem.test(Logit_Buy1$y, Logit_Buy1$fit) #모형 적합도 판단
#(로직 회귀 모형식의 y와 fit 배치)
#알파보다 크게 나와 유의하지 않음 -> 모형 유의함

#문제 5 변수 추가한 분석식 만들기 
Logit_Buy2 <- glm(Buy~Visit+Age+Social,data = Logit,
                  family = binomial() 
)
summary(Logit_Buy2)

#문제 6 이상치 검토
outlierTest(Logit_Buy2)

#문제 7 모형적합도 확인
hoslem.test(Logit_Buy2$y, Logit_Buy2$fit) #hoslem test로 통계적으로 검증

#문제 8 두 모형적합도 비교

#2LL 비교
#deviance -> 2LL
diff <- Logit_Buy1$deviance - Logit_Buy2$deviance
diff

#자유도 비교 (DoF)
dof <-  Logit_Buy1$df.residual-Logit_Buy2$df.residual
dof

#p value 비교
#앞에는 카이스퀘어, 뒤에는 DoF 비교
pchisq(diff,dof) #이 p 카이스퀘어는 p value가 아님
1-pchisq(diff,dof) # p value가 유의하기에 감소량이 유의함. 모형 설명력을 더 높여줌.

#문제 9 예측 accuracy 구하기
prediction1<-predict(Logit_Buy1,newdata = Logit) #예측값 저장
prediction1<-ifelse(prediction1 <0,0,1) #0과 1로 양분화
prediction1 <- as.factor(prediction1) #범주형 척도로 변경

install.packages("caret") #희소행렬 패키지
library(caret)

confusionMatrix(prediction1,Logit$Buy) #DV(y)와 y추정치 비교 
#0인데 1로 오분류한게 4개, 1인데 0으로 오분류한게 5개
#200개 중 9개 오분류, 정분류 191개

#buy2
prediction2<-predict(Logit_Buy2,newdata = Logit) 
prediction2<-ifelse(prediction2 <0,0,1) 
prediction2 <- as.factor(prediction2)

confusionMatrix(prediction2,Logit$Buy) 
#0인데 1로 오분류한게 3개, 1인데 0으로 오분류한게 4개
#200개 중 7개 오분류, 정분류 193개

#buy2의 적합도도 높고, 정확도도 더 높음

#문제 10 새로운 사례 예측하기
C201 <- data.frame(Visit=5, Age=38, Social=11) #case201번
predict(Logit_Buy2,newdata =C201) 
#구매하지 않음.
#결국 음수이기에 0으로 분류될 수 밖에 없음. 따라서 구매하지 않을 것으로 예측 가능




