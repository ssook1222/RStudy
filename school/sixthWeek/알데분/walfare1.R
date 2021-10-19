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

# 나이와 월급 관계
str(welfare7$birth)
summary(welfare7$birth) #이상치 없음

# 나이 파생변수 생성
welfare7 <- welfare7%>%mutate(age=2015-welfare7$birth+1)

# 새롭게 만든 age 변수를 birth 변수 다음으로 이동
welfare7 <- welfare7%>%relocate(age,.after=birth)

# age에 대해 구간을 1로 하는 히스토그램을 그리기
summary(welfare7$age)
hist(welfare7$age,breaks = seq(2,109,by=1))

#나이별로 월급평균을 요약한 데이터 프레임
age_income <- welfare7%>%group_by(age)%>%summarise(mean_income=mean(income,na.rm=T))

#age_income 데이터 프레임 검토
#모든 측정값이 NA이면 분자가 NA가 되므로 mean_income이 NaN이 됨. 
#따라서 mean을 구하기 전 NA를 확인하고 제거해야 함.
age_income<-welfare7%>%filter(!is.na(income))%>%group_by(age)%>%
  summarise(mean_income=mean(income))
#앞에서 filter로 걸렀기 때문에 굳이 na.rm=T로 할 필요 없음.

#나이와 월급의 막대그래프 
ggplot(age_income, aes(age,mean_income,fill=mean_income))+geom_bar(stat="identity")

#연령대별 파생변수(ageg)를 만들고 빈도를 확인하시오.
#mutate와 ifelse 사용
welfare7 <- welfare7%>%mutate(ageg=
  ifelse(age<40, "young", ifelse(
      age<65,"middle","old"
    )
  )
)

table(welfare7$ageg)

#연령대별 월급평균을 요약한 데이터 프레임을 만드시오.
ageg_income<-
  as.data.frame(welfare7%>%group_by(ageg)%>%
  summarise(mean_income=mean(income,na.rm=T)))

#ageg의 순서를 young,middle,old로 변경하시오.
str(ageg_income)
ageg_income$ageg <- factor(ageg_income$ageg,levels = c("young","middle","old"))
#행의 순서를 바꾸는 것이 아니라 범주형으로 바꿔주면서
#factor의 순서를 바꿔주는 것임.

# 성별과 연령대를 모두 고려하여 집단 구분후, 집단별로 월급평균을 요약한 
# 데이터 프레임을 만들고 빈도를 구하시오.
gender_income_ageg <- welfare7%>%group_by(gender,ageg)%>%
  summarise(mean_income=mean(income, na.rm=T))

table(welfare7$ageg)

# 연령대와 월급의 막대 그래프 그리기
ggplot(gender_income_ageg, aes(ageg, mean_income,fill=mean_income))+geom_bar(stat="identity")


