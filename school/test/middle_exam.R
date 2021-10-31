library(dplyr)

#데이터 불러오기
middle <-read.csv("./school/data/middle.csv",fileEncoding ="euc-kr",stringsAsFactors = F)

#문제1
str(middle$date)
#답 :chr (4)

#문제2 
middle$date<-as.Date(middle$date)
#답: as.Date (3)

#문제3
Sys.setlocale("LC_TIME", "ko_KR.UTF-8")
middle$day<-weekdays(middle$date)
#답: weekdays(middle$date)

#문제4
middle<-middle %>% relocate(day, .after = date)
#답: relocate / .after

#문제5
summary(middle)
#rainfall : 272개, customer: 6개

#문제 6
install.packages("lubridate")
library(lubridate)
middle_test <- middle

#8월, 9월 추가
middle_test <- middle_test%>%mutate(month=month(date))

#추출
middle_test%>%filter(month==8|month==9)%>%
  filter(time>=30&claim>=30&error>=10)%>%
  summarise(count=n())

#6

#문제 7
middle_test%>%filter(claim==0&error==0)%>%
  summarise(count=n())

#4

#문제 8
middle_test%>%filter(day=="월요일"|day=="수요일")%>%
  summarise(count=n())

#104

#문제 9
summary(middle$revenue)
hist(middle$revenue,breaks = seq(177207,26000000,by=500000))
#상한: 1000만원, 하한: 500만원

#문제 10
middle_test%>%filter(day=="월요일"|day=="화요일"|day=="수요일"|day=="목요일"|day=="금요일")%>%
  filter(type=="휴일")%>%
  summarise(count=n())

#13

#문제 11
middle_test2 <- middle_test
middle_test2$type <- ifelse((middle_test2$day %in% c("월요일","화요일","수요일","목요일","금요일"))&(middle_test2$type=="휴일"),"공휴일",middle_test2$type)

#공휴일 평균
kong_mean<-middle_test2%>%filter(type=="공휴일")%>%select(customer)%>%summarise(mean_cus=mean(customer))

#휴일 평균
hue_mean<-middle_test2%>%filter(type=="휴일")%>%select(customer)%>%summarise(mean_cus=mean(customer,na.rm=T))

#평일 평균
peong_mean<-middle_test2%>%filter(type=="평일")%>%select(customer)%>%summarise(mean_cus=mean(customer,na.rm=T))

kong_mean-hue_mean+peong_mean

#656.0488

#문제 12
middle_test2$time<- ifelse(middle_test2$time==0,NA,middle_test2)
round(mean(middle_test$time),digits = 2)

#15.36


#문제 13

eventO<-middle_test2%>%filter(event==1)%>%
  select(revenue)%>%
  summarise(mean=mean(revenue))

eventX<-middle_test2%>%filter(event==0)%>%
  select(revenue)%>%
  summarise(mean=mean(revenue))

eventO-eventX

#-3197585


#문제 14
middle_test2<-middle_test2%>%
  mutate(satis=ifelse(satisfaction>=4.75,"매우 만족",
                         ifelse(satisfaction>=4.5&satisfaction<4.75,"만족",
                                ifelse(satisfaction>=4.3&satisfaction<4.5,"보통",
                                       ifelse(satisfaction>=4.0&satisfaction<4.3,"불만족","매우 불만족"
                                       )))))

table(middle_test2$satis)

105+109-83-63+5
#73

middle_test2$satis <- as.factor(middle_test2$satis)


#문제 15
#틀린 문제
middle_test2%>%select(satis,claim)%>%
  summarise(total=sum(claim))%>%
  arrange(claim)%>%head(1)


str(middle_test2)

#문제 16
#틀린 문제
middle_test$time<- ifelse(middle_test$time==0,NA,time)
middle_test<-middle_test%>%
  mutate(
    problem=time+claim+error
  )

table(!is.na(middle_test$problem))


