exam<-read.csv("./school/data/exam.csv",fileEncoding ="euc-kr",stringsAsFactors = F)
library(dplyr)
library(ggplot2)
#id 열 생성
exam$id<-NULL
v1 <- c(1:30) #1~30까지의 정수를 v1이라는 벡터에 지정
exam<-exam %>% mutate(id = v1) #id 새로 지정

#relocate
exam<-exam %>% relocate(id, .before = address) #.after도 적용가능

#left-join 함수 실습
exam_science<-read.csv("./school/data/exam_science.csv",fileEncoding ="euc-kr",stringsAsFactors = F)
exam <- left_join(exam,exam_science,by='id') #데이터프레임과 파이프연산자를 사용할 필요 없음
exam <- exam %>% mutate(total=english+math+history)
exam<-exam %>% relocate(Science, .before = total) #.after=history와 동일

#18-1
mpg<-as.data.frame(mpg)
mpg<-rename(mpg,fuel=fl,city=cty,highway=hwy)
mpg$fuel<-ifelse(mpg$fuel=="c","CNG",
                 ifelse(mpg$fuel=="e","ethanol",
                        ifelse(mpg$fuel=="d","diesel",
                               ifelse(mpg$fuel=="p","premium",
                                      ifelse(mpg$fuel=="r","regular",mpg$fuel)))))
mpg$fuel<-as.factor(mpg$fuel)
fuel_price<-data.frame(fuel=c("CNG", "diasel","ethanol","premium","regular"),
                       fuel_price=c(2.35,2.38,2.11,2.76,2.22
                                    ))
fuel_price$fuel<-as.factor(fuel_price$fuel)

#18-2
mpg<-left_join(mpg,fuel_price,by='fuel')

#문제 19-1
mpg$drv<-ifelse(mpg$drv=="f","forward",ifelse(mpg$drv=="r","rear",mpg$drv))
drv_price<-data.frame(driving=c(4,"forward","rear"),driving_price=c(40000,30000,50000))

#문제 19-2
mpg<-left_join(mpg,drv_price,by=c("drv"="driving"))

#문제 20
mpg<-mpg %>% relocate(fuel_price, .after = fuel)
mpg<-mpg %>% relocate(driving_price, .before = city)

#bind-rows 함수 실습
exam<-exam%>%mutate(total=math+english+history)
exam<-exam%>%mutate(average=total/3)
exam<-exam%>%mutate(test=ifelse(total>=180, "pass","fail"))
exam_add <- read.csv("./school/data/exam_add.csv",fileEncoding ="euc-kr",stringsAsFactors = F)
exam <- bind_rows(exam,exam_add)
exam <- exam %>% distinct(id,total,.keep_all = T)

#문제 21
exam <- exam %>% distinct(total,class,math,english,history,.keep_all = T)

#문제 22
exam$average<-ifelse(is.na(exam_new$average),exam_new$total/3,exam_new$average)
exam$average<-round(exam$average,digits = 2)

#문제 23
exam$Science<-ifelse(is.na(exam$Science),round(mean(exam$Science,na.rm = T),digits = 2),exam$Science)



