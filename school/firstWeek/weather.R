weather<-read.csv("./school/data/weather.csv",fileEncoding ="euc-kr",stringsAsFactors = F)

str(weather)

weather$일시<-as.Date(weather$일시)
weather$요일.구분<-as.factor(weather$요일.구분)
str(weather)

weather$요일<-weekdays(weather$일시)
weather$요일<-as.factor(요일)
