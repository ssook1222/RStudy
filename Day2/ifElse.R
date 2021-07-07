install.packages("ggplot2")
# mpg 데이터 불러와서 저장
mpg<-ggplot2::mpg
# 통합(total) 연비 변수 생성 
mpg$total<-(mpg$cty+mpg$hwy)/2
head(mpg)

#통합 연비 변수 평균
mean(mpg$total)

#Case1. 조건문 사용해 파생변수 만들기

#1.기준값 정하기
summary(mpg$total)
hist(mpg$total)

#2. 조건문
mpg$test<-ifelse(mpg$total>=20, "pass","fail")

#3. 데이터 확인
head(mpg,20)

#4. 빈도표로 합격 판정 자동차 수 확인
table(mpg$test)

#5. 시각화
library('ggplot2')
qplot(mpg$test)


#Case2. 중첩 조건문 활용하기
#1. total 기준으로 등급 부여
mpg$grade<-ifelse(mpg$total>=30,"A",
                    ifelse(mpg$total>=20,"B","C"))
#2. 데이터 확인
head(mpg,20)

#3.등급 빈도표 생성
table(mpg$grade)
qplot(mpg$grade)

#4. 원하는 만큼 범주 만들기
mpg$grade2<-ifelse(mpg$total>=30,"A",
                   ifelse(mpg$total>=25,"B",
                                            ifelse(mpg$total>=20,"C","D")))
table(mpg$grade2)
qplot(mpg$grade2)
