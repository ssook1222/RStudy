library(dplyr)

#엑셀 파일 불러오기
library(readxl)
list_job <- read_excel("./school/data/2015 codebook.xlsx",sheet = 2)

#join하기 전 기준이 되는 열의 데이터 타입 확인하기
#데이터 타입이 다르면 에러가 발생
str(welfare7$job)
str(list_job$job)
#타입이 다른 경우 데이터 타입을 맞춰줘야 함.
list_job$job <- as.factor(list_job$job)
#join 진행
welfare7 <- left_join(welfare7,list_job,by="job")
#열 위치 이동
welfare7 <- welfare7%>%relocate(title, .after = job)

table(welfare7$job)

#job의 빈도수(112~부터는 없음)
table(is.na(welfare7$job))

# 소득이 있는 직업 케이스만 추출
# 직업별 월급 평균을 보여줌
title_income <- welfare7 %>% filter(!is.na(title)&!is.na(income))%>%
  group_by(title)%>%
  summarise(mean_income=mean(income))

#상위 5개 직업(내림차순)
title_income%>%arrange(-mean_income)%>%head(5)

#하위 5개 직업(오름차순)
title_income%>%arrange(mean_income)%>%head(5)

#직업명에 따른 월급평균 데이터 프레임 생성
library(ggplot2)
title_income20<-title_income%>%arrange(-mean_income)%>%head(20)
ggplot(title_income20,aes(title,mean_income,fill=mean_income))+
  geom_bar(stat="identity")+theme(text=element_text(size = 12,family = "NanumGothic"))

ggplot(title_income20,aes(reorder(title,-mean_income),mean_income,fill=mean_income))+
  geom_bar(stat="identity")+theme(axis.text.x = element_text(size = 7.5,angle=50,family = "NanumGothic"))


#성별과 직업 관계
#남성의 상위 직업 빈도 20개 테이블 만들기 (빈도: count = n())
title_male <- welfare7 %>% 
  filter(gender=="male"&!is.na(title))%>%
  group_by(title)%>%
  summarise(count=n())%>%
  arrange(-count)%>%
  head(20)

#여성의 상위 직업 빈도 20개 테이블 만들기 (빈도: count = n())
title_female <- welfare7 %>% 
  filter(gender=="female"&!is.na(title))%>%
  group_by(title)%>%
  summarise(count=n())%>%
  arrange(-count)%>%
  head(20)

#지역과 연령 관계
#타입 확인
str(welfare7$region)

#척도 변경
welfare7$region <- as.factor(welfare7$region)

#빈도 확인
table(welfare7$region)

#데이터 프레임 생성(변경이 아닌 생성!)
region_list <- data.frame(region=c(1:7),region_name=c("서울",
                                                      "수도권",
                                                      "부울경",
                                                      "대경",
                                                      "대전충남",
                                                      "강원충북",
                                                      "호남제주"))

#welfare7과 region_list 데이터 프레임 통합
#타입 확인
str(region_list$region)
region_list$region <- as.factor(region_list$region)
welfare7<-left_join(welfare7,region_list,by="region")

#변수 이동
welfare7<-welfare7%>%relocate(region_name,.after = region)

#지역별로 평균 연령 구하기
welfare7 %>% 
  group_by(region_name)%>%
  summarise(mean_age=mean(age))

#**지역별로 연령대 비율 구하기

#case 체크
welfare7%>%group_by(region_name,ageg)%>%
  summarise(count=n())

#지역 내 연령대 비율
welfare7%>%group_by(region_name,ageg)%>%
  summarise(count=n())%>%
  mutate(total_subset=sum(count))%>%
  mutate(rate=count/total_subset)

#전체 사례 대비 연령대 비율
welfare7%>%group_by(region_name,ageg)%>%
  summarise(count=n(),rate=count/16664)

#하드 코딩 없이
welfare7%>%group_by(region_name,ageg)%>%
  summarise(count=n(), rate=count/nrow(welfare7))







