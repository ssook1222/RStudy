library(dplyr)
competition <- read.csv("./school/data/final1_data.csv",stringsAsFactors = F,fileEncoding = "euc-kr")
str(competition)

competition$attend<- ifelse(competition$attend=="참가",1,0)
competition$attend <- as.factor(competition$attend)

library(mlogit)
logit_comp <- glm(attend~age+past,
                  data = competition,
                  family = binomial() #DV가 이진 분포임을 알림
)
summary(logit_comp) 
#알파(a 0.5)보다 0.000287 0.000117으로 작아서 유의 
#둘 다 유의 age는 -0.8817만큼, past는 1.2119만큼 미침

library(ResourceSelection)
hoslem.test(logit_comp$y,logit_comp$fit) #p value가 1로 유의하지 않으므로 모형적합도는 유의
#x squared 역시 1-0.33688 : 0.66312으로 유의하지 않으므로 모형 적합

logit_comp2 <- glm(attend~age+past+sns,
                  data = competition,
                  family = binomial() #DV가 이진 분포임을 알림
)
summary(logit_comp2) #0.5899만큼 영향을 미침. p value가 유의수준 0.5보다 작은 0.02085이므로 그냥 진행해도 됨

diff<- logit_comp$deviance-logit_comp2$deviance
dof <- logit_comp$df.residual - logit_comp2$df.residual
1-pchisq(diff,dof)
#카이제곱 검정 결과 0.001693754로 유의수준 0.5보다  0.001693754로 작게 나와 유의하게 나옴. 모형 적합도가 증가함.

prediction1 <- predict(logit_comp,newdata = competition)
prediction1 <- ifelse(prediction1<0,0,1)
prediction1 <- as.factor(prediction1)
library(caret)

confusionMatrix(prediction1, competition$attend) #0.9667 -> 0.967

c181 <- data.frame(age=36, past=5, sns=8)
c182 <- data.frame(age=38, past=8, sns=12)

predict(logit_comp2,newdata = c181) #예측 결과 음수(-0.205)가 나와 <0.5이므로 참여 안 함.
predict(logit_comp2,newdata = c182) #예측 결과 양수(4.210)가 나와 >0.5이므로 참여함.

shopping <- read.csv("./school/data/final2_data.csv",stringsAsFactors = F,fileEncoding = "euc-kr")

library(NbClust)
set.seed(123)
nc_shop <- NbClust(shopping,distance = "euclidean",min.nc=3,max.nc=5, method="average")
nc_kmeans <- kmeans(shopping,centers=5, nstart=25)
nc_kmeans$size #25 31 23 27 14 -> 31 27 25 23 14

aggregate(shopping,by=list(cluster=nc_kmeans$cluster),mean)
# 1번 군집은 기혼이 많고(1.96) 40대 이상(4.2)이 대다수 주말에 쇼핑 많이 함(3.36).
# 2번 군집은 대다수 미혼(1.194)이고 20대(2.161)이고 금요일 ~ 토요일(2.64) 쇼핑 많이
# 3번 군집은 기혼이 많고(1.782) 40대 이상(4.087)이 대다수 평일에 쇼핑 많이 함(1.304).
# 4번 군집은 기혼이 많고(1.852) 40대 이상(4.037)이 주말에 쇼핑 많이 함(3.555).
# 5번 군집은 기혼이 많고(1.929) 40대 이상(4.143)이 대다수 평일에 쇼핑 많이 함(1.429).

banking <- read.csv("./school/data/final3_data.csv",stringsAsFactors = F,fileEncoding = "euc-kr")
str(banking)
banking$deposit <- as.factor(banking$deposit)
banking_z<- as.data.frame(scale(banking[,-1]))
banking_z$deposit <- banking$deposit

table(banking$deposit)

set.seed(123)

ind <- sample(2,2500,replace = T,prob = c(0.8,0.2))
banking_train <- banking_z[ind==1,] 
banking_test <- banking_z[ind==2,]

library(caret)
grid1 <- expand.grid(k=4:6)
control<- trainControl(method="repeatedcv",number = 10 #fold 개수
                       ,repeats = 5)

set.seed(5678)
knn_train <-train(deposit~.,data = banking_train,method="knn",trControl=control
                  ,tuneGrid=grid1)
knn_train #k=6
varImp(knn_train,scale = F) #가장 중요한 변수 number , Importance가 0.806

pred_train <- predict(knn_train,newdata = banking_train)
confusionMatrix(pred_train,banking_train$deposit) 
#Accuracy : 0.8959   Kappa : 0.3862 

pred_test <- predict(knn_train,newdata = banking_test)
confusionMatrix(pred_test,banking_test$deposit) 
#Accuracy : 0.8699이지만 kappa가 Kappa : 0.1927로 낮음
#test로 했을 때 정확도 0.026 감소, 0.1935 감소로 현저하게 작아서 과적합일 가능성이 큼

#한계: k 설정이 중요하다. 데이터의 분포가 압도적(7배)으로 차이날 때 예측력이 떨어진다.

pred2_test <- read.csv("./school/data/final3_prediction.csv",stringsAsFactors = F,fileEncoding = "euc-kr")
pred2_test$id <- NULL
pred2_test <- as.data.frame(scale(pred2_test))

pred_test2 <- predict(knn_train, newdata = pred2_test) 
pred_test2 #no  no  no  no  no  yes

library(e1071)
set.seed(34567)
rbf_svm <- tune.svm(deposit~.,data = banking_train,kernel="radial",
                    gamma = seq(0.01,1,by=0.05))
summary(rbf_svm) #0.06 0.871(정확도)
rbf_svm$best.model #562개

rbf_train <- predict(rbf_svm$best.model, newdata = banking_train)
rbf_test <- predict(rbf_svm$best.model, newdata = banking_test)
confusionMatrix(rbf_test,banking_test$deposit) #0.8902, 0.1124
confusionMatrix(rbf_train,banking_train$deposit) #0.88, 0.1558
#test 결과가 더 좋아서(정확도 0.01 증가 kappa 0.0434) 과적합 위험 X

svm_predict <- predict(rbf_svm$best.model,newdata = pred2_test)
svm_predict 
#no no no no no no로, 마지막 6번 사례가 다름.



