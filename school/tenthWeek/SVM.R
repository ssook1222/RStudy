#step1. 데이터 프레임 생성 및 전처리
#데이터 프레임 생성
library(dplyr)
cancer<-read.csv("./school/data/wisc_cancer_data.csv",stringsAsFactors = F)

#데이터 전처리
str(cancer)
cancer$diagnosis <- as.factor(cancer$diagnosis)
cancer$id <- NULL

cancer_svm <- as.data.frame(scale(cancer[-1])) #DV제외
cancer_svm$diagnosis <- cancer$diagnosis

#step2 . 데이터 셋 구성
set.seed(123)
ind <- sample(2,569,replace=T,prob=c(0.7,0.3))

cancer_svm_train = cancer_svm[ind==1,]
cancer_svm_test = cancer_svm[ind==2,]

table(cancer_svm_train$diagnosis)
table(cancer_svm_test$diagnosis)
#비율 확인하고 비율 차가 많이 나면 다시 시도

#step3. svm 모델 생성
install.packages("e1071")
library(e1071)

linear_svm <- tune.svm(diagnosis~.,data = cancer_svm_train,kernel="linear",
                       cost=c(0.01,0.05,0.1,0.25,0.5,1,2,3,5,10))
#cost 디폴트 1

summary(linear_svm) # 0.1일 때가 best임. 0.0197
linear_svm$best.model #50개

#step 4. test데이터 성능 평가
linear_test <- predict(linear_svm$best.model,newdata = cancer_svm_test) #bestmodel에 적용
library(caret)
confusionMatrix(linear_test,cancer_svm_test$diagnosis) 
#만든 모델과 실제 예측 값의 정확도가 0.01 차이로 작음. 따라서 Overfit 아님.
#정확도 :  0.9817, kappa : 0.9601

#step5. 커널 반복
# polynomial kernel

poly_svm1 <- tune.svm(diagnosis~.,data = cancer_svm_train,kernel="polynomial",
                     degree = c(3,4,5,6) #차원
                     ,gamma=seq(0.5, 5, by=0.5) #0.5부터 5까지 0.5간격
                     ,coef0=seq(0.5, 5, by=0.5)
                     )
#코스트 잡아보기
poly_svm2 <- tune.svm(diagnosis~.,data = cancer_svm_train,kernel="polynomial",
                     degree = c(3,4,5) #차원
                     ,gamma=seq(0.5, 4, by=0.5) #0.5부터 5까지 0.5간격
                     ,coef0=seq(0.5, 4, by=0.5)
                     ,cost=c(0.1,0.5,1,2)
)

summary(poly_svm1) # degree 3, gamma 0.5, coef0 3.5가 최고 / 정확도 0.9534
poly_svm1$best.model

summary(poly_svm2) # degree 4 / gamma 0.5 / coef0 2 / cost 0.1
poly_svm2$best.model

# test data를 활용한 성능 평가
poly_test <- predict(poly_svm$best.model,newdata = cancer_svm_test)
confusionMatrix(poly_test,cancer_svm_test$diagnosis) #kappa가 더 낮음

#rbf kernel 활용
rbf_svm <- tune.svm(diagnosis~.,data = cancer_svm_train,kernel="radial",
                       gamma = seq(0.05,1,by=0.05)) #cost를 굳이 잡을 필요 없음

summary(rbf_svm) #gamma가 작을수록 좋았던 예제. gamma=0.05
rbf_svm$best.model

rbf_test <- predict(rbf_svm$best.model, newdata = cancer_svm_test)
confusionMatrix(rbf_test,cancer_svm_test$diagnosis) #아직도 선형 초평면이 제일 나음


#sigmoid kernel trick
sigmoid_svm <- tune.svm(diagnosis~.,data=cancer_svm_train, kernel="sigmoid",
                        gamma = seq(0.01,1,by=0.02),coef0 = seq(0.5,4,by=0.5))
summary(sigmoid_svm) #gamma 0.01 coef0 0.5
sigmoid_svm$best.model

sigmoid_test <- predict(sigmoid_svm$best.model,newdata = cancer_svm_test)
confusionMatrix(sigmoid_test,cancer_svm_test$diagnosis) #크기가 더 줄긴 했음.

#가장 좋은 것은 Linear

#step5. linear 성능 개선

linear_svm <- tune.svm(diagnosis~.,data = cancer_svm_train,kernel="linear",
                      cost=seq(0.01,10,by=0.05)
)
summary(linear_svm)
linear_svm$best.model

linear_test <- predict(linear_svm$best.model,newdata = cancer_svm_test) #bestmodel에 적용
confusionMatrix(linear_test,cancer_svm_test$diagnosis) 

#step6. 예측
cancer_predict<-read.csv("./school/data/wisc_cancer_prediction.csv",stringsAsFactors = F)
cancer_predict$id <- NULL

cancer_predict <- as.data.frame(scale(cancer_predict))
svm_predict <- predict(linear_svm$best.model,newdata = cancer_predict)
svm_predict #KNN과 SVM 결과 동일

























