#spss 파일인 .sav 파일을 불러오기 위해 foreign 패키지 설치
install.package("foreign")
library(foreign)

#데이터 프레임 생성
raw_welfare<-read.spss(file="school/data/2015 welfare.sav",reencode='euc-kr',to.data.frame = T)
welfare<-raw_welfare

#변수명 변경
library(dplyr)
welfare<-welfare%>%rename(
  gender=h10_g3, birth=h10_g4, marriage=h10_g10,religion=h10_g11,
  job=h10_eco9,income=p1002_8aq1,region=h10_reg7
)

#7개 변수로만 구성된 데이터 프레임 만들기
welfare7<-welfare%>%select(gender,birth, marriage, religion, job, income, region)


# 성별 변수 전처리
# 성별 행 값 변경
welfare7$gender<-ifelse(welfare7$gender==1,"male","female")

# gender 범주형으로
str(welfare7$gender)
summary(welfare7$gender)
welfare7$gender<-as.factor(welfare7$gender)

# gender 변수에 대한 결측치나 이상치 여부를 확인하시오.
table(welfare7$gender)
table(is.na(welfare7$gender))

# income 변수에 대해 50먼원 구간으로 히스토그램을 그리시오.
# 히스토그램을 그리기 위한 최대, 최소 확인
summary(welfare7$income)
# 히스토그램 생성
hist(welfare7$income,breaks = seq(0,2400,by=50))

#income이 0인 빈도를 확인하여, NA로 바꾸고 NA 빈도를 구하시오.
# 0인 빈도 확인
table(welfare7$income==0)
# NA로 바꾼 후 NA 빈도 구하기
welfare7$income<-ifelse(welfare7$income==0,NA,welfare7$income)
# NA 빈도 구하기
table(is.na(welfare7$income))


# 성별에 따른 월급평균을 요약한 데이터 프레임(gender_income) 만들기
gender_income<-welfare7%>%group_by(gender)%>%summarise(mean_income=mean(income, na.rm=T))


# gender_income에 대한 막대 그래프 그리기
library(ggplot2)
ggplot(gender_income,aes(gender, mean_income, fill=mean_income))+geom_bar(stat="identity")
# ggplot(gender_income, aes(X축 변수, Y축 변수, fill=Y축 변수))+geom_bar(stat="identity")








