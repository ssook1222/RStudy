weather<-read.csv("./school/data/weather.csv",fileEncoding ="euc-kr",stringsAsFactors = F)

str(weather)

weather$일시<-as.Date(weather$일시)
weather$요일.구분<-as.factor(weather$요일.구분)
str(weather)

weather$요일<-weekdays(weather$일시)
weekdays(weather$일시)
weather$요일<-as.factor(weather$요일)

summary(weather)

var(weather$일강수량,na.rm=T)

table(weather$요일.구분)
weather$요일<-factor(weather$요일,levels=c("월요일","화요일","수요일","목요일","금요일","토요일","일요일"))
table(weather$요일)

library(ggplot2)
qplot(data=weather,요일,fill=요일.구분)+theme(text=element_text(size = 12,family = "NanumGothic"))

hist(weather$평균기온, breaks = seq(-20,50,by=1))
hist(weather$평균.상대습도, breaks = seq(0,100,by=1))

install.packages("pastecs")
library(pastecs)

install.packages("psych")
library(psych)

stat.desc(weather$평균기온)
describe(weather$평균기온)
