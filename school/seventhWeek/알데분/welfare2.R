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



