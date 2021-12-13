#step1

#데이터 프레임 생성
library(dplyr)
cancer<-read.csv("./school/data/wisc_cancer_data.csv",stringsAsFactors = F)

#데이터 전처리
str(cancer)
summary(cancer)
cancer$diagnosis <- as.factor(cancer$diagnosis)
cancer$id <- NULL

#종속변수를 제외하고 전부 데이터 표준화
cancer_z<- as.data.frame(scale(cancer[,-1])) #모든 행에 대해 첫번째 변수(종속변수)만 제외하고 정규화해라
#만든 데이터 프레임에 종속변수 복사하기
cancer_z$diagnosis <- cancer$diagnosis

#step2
#set.seed 하고 sample 해야 결과가 그대로 나옴
set.seed(123)
ind <- sample(2,569,replace = T,prob = c(0.7,0.3)) #추출 대상, 추출 횟수, 비복원/복원 여부, 추출 대상의 비율
#여기서는 1과 2(대상 -> 테스트 데이터 셋 번호를 랜덤으로 부착) 중 569번 복원 추출

ind
cancer_z[ind==1,31] #ind의 결과값이 1인 것에 대한 diagnosis 출력
cancer_z[ind==2,31] #ind의 결과값이 2인 것에 대한 diagnosis 출력

#7,3의 비율로 데이터셋 생성
cancer_train <- cancer_z[ind==1,]
cancer_test <- cancer_z[ind==2,]

#균형이 너무 어긋나면 set.seed부터 다시 시작하기
table(cancer_z$diagnosis) #M의 비율 : 37.26%
table(cancer_train$diagnosis) #M의 비율: 37.78%
table(cancer_test$diagnosis) #M의 비율: 35.97%

#step3 model training & 최적의 값 산출
library(caret)
grid1 <- expand.grid(k=3:10) #knn의 k 후보군(3~10) 지정

#학습방법 지정
control<- trainControl(method="repeatedcv",number = 10 #fold 개수
             ,repeats = 5) #10 fold, 5번 반복, cross validation

set.seed(5678)
knn_train <-train(diagnosis~.,data = cancer_train,method="knn",trControl=control
                  ,tuneGrid=grid1) #종속변수~독립변수. 방법, 학습 방법, k의 후보군   

knn_train
plot(knn_train)

varImp(knn_train,scale = F) #knn에서 가장 중요한 변수

#step4 검증 데이터를 이용한 성능 평가
library(caret)
pred_test <- predict(knn_train,newdata = cancer_test)
pred_test #예측 결과 : 음성, 양성을 보여줌
cancer_test$diagnosis #실제 결과
confusionMatrix(pred_test,cancer_test$diagnosis)#예측결과와 실제 결과 비교
#만약 위의 실행결과가 정확도와 kappa가 step3 값보다 현저하게 작아질 경우 과적합 우려
#과적합이 우려되면 모델 생성부터 다시, 아니면 모델 비교

#step5 성능개선
install.packages("kknn") #가중치를 부여한 knn
library(kknn)

set.seed(1357) #난수를 생각하는 게 있어서 set.seed 필수
kknn_train <- train.kknn(diagnosis~.,data=cancer_train #train data를 활용한 모델 훈련
                         ,kmax=25, distance=2,kernel = c("rectangular", "triangular", "Epanechnikov", "biweight", "triweight", 
                                                         "cosine", "inversion", "Gaussian", "rank", "optimal"))
                          #k 최대, 유클리디안 방법, 커널(가중치 부여법-훈련방법) 

kknn_train
kknn_pred_test <- predict(kknn_train,newdata=cancer_test)
confusionMatrix(kknn_pred_test,cancer_test$diagnosis) #크게 개선되지 않음

#step6. 예측
cancer_predict<-read.csv("./school/data/wisc_cancer_prediction.csv",stringsAsFactors = F)
cancer_predict$id <- NULL
cancer_predict <- as.data.frame(scale(cancer_predict))

#1 knn_train 예측(과적합 발생 X여서 함께 사용)
pred_test2 <- predict(knn_train, newdata = cancer_predict) 

#2 kknn_train 예측(과적합 발생하면 얘만 사용)
kknn_pred_test2 <- predict(kknn_train, newdata = cancer_predict) 

pred_test2
kknn_pred_test2
#결과가 일치함.









