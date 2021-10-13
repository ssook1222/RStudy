library(ggplot2)
library(dplyr)

# 문제 1 : ggplot2에서 midwest 데이터를 불러와서 같은 이름의 데이터 프레임을 만드시오. 
midwest<-as.data.frame(midwest) #티블을 데이터 프레임으로 변경

# 정답 : 라이브러리에서 바로 데이터 불러오기
midwest <- ggplot2::midwest 
midwest<-as.data.frame(midwest)
# 문제 2 : popadults는 해당 지역 성인 인구, poptotal은 해당 지역 전체 인구를 의미한다.
#지역별 미성년 인구비율 변수를 만드시오.
midwest<-midwest%>%mutate(percyouth=((poptotal-popadults)/poptotal)*100)
midwest$percyouth<-round(midwest$percyouth,digits = 2)

# 문제 3
midwest%>%select(county,percyouth)%>%arrange(-percyouth)%>%head(5)

(midwest%>%arrange(-percyouth)%>%head(5))$county

# 문제 4
midwest<-midwest%>%mutate(group=ifelse(percyouth>=40,"large",ifelse
                                       (percyouth>=30&percyouth<40,"middle","small")))
table(midwest$group)

# 문제 5
midwest_add<-read.csv("./school/data/midwest_add.csv",fileEncoding ="euc-kr",stringsAsFactors = F)
midwest_add2<-read.csv("./school/data/midwest_add2.csv",fileEncoding ="euc-kr",stringsAsFactors = F)

midwest_test<-left_join(midwest,midwest_add,by=c("county"="region"))
midwest_check<-left_join(midwest,midwest_add2,by=c("PID"="PID"))

table(is.na(midwest_test$senior)) #결측치 확인
table(is.na(midwest_check$senior)) #결측치 확인

# 문제 6
midwest_test<-midwest_test%>%distinct(PID,county,state,.keep_all = T)

# 문제 7
table(midwest$state)
midwest_test%>%group_by(state)%>%summarise(sum_senior=sum(senior,na.rm = T))

midwest_test%>%filter(!is.na(senior))%>%group_by(state)%>%summarise(sum_senior=sum(senior))

# 문제 8
library(tidyr)
# midwest<-midwest%>%drop_na()
midwest_test<-midwest_test%>%drop_na()
midwest_check <- midwest_check%>%drop_na()

# 문제 9
table(midwest$category)
midwest%>%select(category,popasian)%>%arrange(mean(popasian))%>%head(3)
(midwest%>%select(category,popasian)%>%arrange(mean(popasian))%>%head(3))$category

