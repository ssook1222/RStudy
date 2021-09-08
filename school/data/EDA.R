exam<-read.csv("./school/data/exam.csv",fileEncoding ="euc-kr",stringsAsFactors = F)

#데이터 파악용 함수
head(exam)
tail(exam)

head(exam,10)
tail(exam,8)

#특정 변수를 지정해서 해당 변수의 데이터 파악하기
head(exam$gender,7)

View(exam)

#특정 변수(열 제거)
exam$id<-NULL

#행과 열 확인
dim(exam)

#데이터 구조 확인 가능
str(exam)

#척도 변경(변수 타입 변경) : 여기서는 전부 범주형(factor) 척도로 변경
exam$address <- as.factor(exam$address) #6,4,5,1...: 레벨별 부여되는 숫자
exam$gender <- as.factor(exam$gender)
exam$class <- as.factor(exam$class)

summary(exam)

#개별 기술통계량 함수 사용하기
mean(exam$math)

#유효숫자 지정(소수점 자리 지정)
round(mean(exam$math), digits = 2)

#분산, 표준편차 구하기
var(exam$math)
sd(exam$math)

var(exam$history)
sd(exam$history)


#히스토그램 그리기
hist(exam$math)
hist(exam$math, breaks = seq(0,100,by=5))
hist(exam$history, breaks = seq(0,100,by=5))
hist(exam$english, breaks = seq(0,100,by=5))

#테이블과 qplot을 사용한 빈도수 파악하기
table(exam$address)
table(exam$class)
table(exam$gender)

#변수 2개에 따른 빈도수 확인하기
table(exam$address, exam$gender)
table(exam$class, exam$address) #반(앞 변수) 별 주소(뒷 변수)

#qplot 패키지 설치
install.packages("ggplot2")
library(ggplot2)

#막대 그래프 형태로 표현
qplot(data=exam,address)+theme(text=element_text(size = 12,family = "NanumGothic"))
qplot(data=exam,address,fill=gender)+theme(text=element_text(size = 12,family = "NanumGothic"))
qplot(data=exam,class,fill=address)+theme(text=element_text(size = 12,family = "NanumGothic"))


