weather<-read.csv("./school/data/weather.csv", fileEncoding = "EUC-KR")

# 문제 1
table(weather$평균기온>=27)

# 문제 2
table(weather$평균기온>=10&weather$평균기온<=20)

# 문제 3
table(weather$일강수량==0)

# 문제 4 결측치
summary(weather$일강수량)
table(is.na(weather$일강수량))
# cf) 일강수량이 결측치가 아닌 것 table(!is.na(weather$일강수량))

# 문제 5
Sys.setlocale("LC_TIME", "ko_KR.UTF-8")
weather$일시<-as.Date(weather$일시)
weather$요일<-weekdays(weather$일시)

weather$요일<-as.factor(weather$요일)
weather$요일<-factor(weather$요일,levels=c("월요일","화요일","수요일","목요일","금요일","토요일","일요일"))

table(weather$요일=="월요일"|weather$요일=="화요일"|weather$요일=="수요일")
table(weather$요일 %in% c("월요일","화요일","수요일"))

# 문제 6
table(weather$최고기온>30&weather$평균.상대습도>80)

# 문제 7
table(weather$최저기온< -10|weather$합계.일조시간<1.0)


weather_new<-weather

# 문제 8
library(dplyr)
weather_new<-rename(weather,요일구분=요일.구분, 평균기압=평균.현지기압)

# 문제 9
table(weather$요일.구분) #before

weather_new$요일구분<-factor(weather_new$요일구분,levels=c("휴일","평일"))
table(weather_new$요일구분) #after

# 문제 10
weather_new$일강수량<-ifelse(weather_new$일강수량==0,NA,weather_new$일강수량)

# 문제 11
table(is.na(weather_new$평균기압))

# 문제 12
round(mean(weather_new$평균기압,na.rm=T),digits=2)

# 문제 13
weather_new$평균기압<-ifelse(is.na(weather_new$평균기압)==TRUE,
                         round(mean(weather_new$평균기압,na.rm=T),digits=2)
                         ,weather_new$평균기압)






