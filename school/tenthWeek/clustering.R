library(dplyr)

#계층적 군집분석

#step1~2 전처리
lm_order_h <- lm_order%>%select(duration,order,morning, expense, payment, age, gender, os
                                , satisfaction)
str(lm_order_h)
lm_order_h$gender <- ifelse(lm_order_h$gender=="Female",1,2)
lm_order_h$os <- ifelse(lm_order_h$os=="Android",1,2)
#척도가 numeric으로 바뀌어도 상관 X

#step3 더미변수
#기준집단 정하기 -> 계좌이체
#기준집단을 제외한 나머지 더미변수를 새로 만드므로 mutate 생성
lm_order_h <- lm_order_h%>%mutate(easy=ifelse(payment=="간편결제",2,1))
lm_order_h <- lm_order_h%>%mutate(card=ifelse(payment=="신용카드",2,1))

#그 후 원래 payment 삭제
lm_order_h$payment <- NULL

#step4 거리 구하기
install.packages("cluster")
library(cluster) #거리 구할 때 사용

daisy(lm_order_h, metric = "gower") #gower로 해줘야 연속형 데이터는 맨허튼을
d <- daisy(lm_order_h, metric = "gower")
#이산형 데이터는 다이스거리를 계산함
#이 데이터 프레임의 모든 변수를 사용할 것

#step 5 계층적 군집분석 진행 
cls <- hclust(d,method="ward.D2") #ward 방법, 군집 내의 사례 간의 차이(=분산)을 최소화하고, 
#서로 다른 군집에 속한 사례간 차이를 최대화하는 방법
#대안으로 쓸 수 있는 방법은 average, hclust(d,method="average")

#덴드로그램 그리기
plot(cls, hhang=-1, cex=0.9, col="darkgreen",xlab="customer", main="hclustering")

#step 6 군집 개수 결정
install.packages("NbClust")
library(NbClust)

set.seed(123) #숫자는 아무거나 사용

NbClust(lm_order_h,distance = "euclidean",min.nc=3,max.nc=12, method="average")

nc_h <- NbClust(lm_order_h,distance = "euclidean",min.nc=3,max.nc=12, method="average")
#결과의 According to the majority rule, the best number of clusters is를 확인하면 됨

table(nc_h$Best.nc[1,]) #군집개수 4개일 경우 가장 많은 지지를 받음, 옵션임. 안해도 됨

#step7 군집 만들기
hcl <- cutree(cls,4)
table(hcl) #군집별로 어디에 속했는지 보여주는 열벡터

#step8 평균값 구해서 비교
aggregate(lm_order_h,by=list(cluster=hcl),mean) #각 클러스터별 특징을 평균으로 확인 가능

#K-means 군집 분석

#step1 연속형 데이터만 추출
lm_order_k <- lm_order %>% select(-case, -ID, -path, -payment, -gender, -os, -return_YN)
#step2 정규화
lm_order_k <- scale(lm_order_k)
#step3 군집 개수 지정 및 군집화 진행
set.seed(123)
nc_k <- NbClust(lm_order_k,distance = "euclidean",min.nc=3, max.nc = 12, method="kmeans") #3
table(nc_k$Best.nc[1,]) #적절한 군집 수 투표한 거 확인

set.seed(123)
cls_k <- kmeans(lm_order_k,centers=3, nstart=25) #centers = 3, nstart= 군집 생성 횟수
cls_k$size
head(cls_k$cluster) #cluster : 어느 군집에 속하는 지 보여줌

#step4 군집별 평균 구한 후 비교
aggregate(lm_order_k,by=list(cluster=cls_k$cluster),mean)
clusplot(x=lm_order_k,clus=cls_k$cluster,color=T,shade=T,labels = 2, lines = 0, 
         main="k-means clustering") #군집화 그래프 생성

#추가 통합 -> 어느 클러스터에 들어있는지 결과값을 데이터 프레임에 병합
lm_order_k <- cbind(lm_order_k,cls_k$cluster)



