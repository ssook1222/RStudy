exam<-read.csv("./school/data/exam.csv",fileEncoding ="euc-kr",stringsAsFactors = F)
library(dplyr)
library(ggplot2)

#mutate함수
exam<-exam%>%mutate(total=math+english+history)
exam<-exam%>%mutate(average=total/3)
exam<-exam%>%mutate(total=math+english+history,average=total/3)
exam$average<-round(exam$average,digit=2) #그냥 exam 쓰면 다 날아감 

#mutate와 ifelse 결합
exam<-exam%>%mutate(test=ifelse(total>=180, "pass","fail"))
table(exam$test)

#문제 11
mpg<-mpg%>%mutate(sum=cty+hwy)

#문제 12
mean(mpg$sum)

#문제 13
mpg%>%
  mutate(avg=sum/2)%>%
  arrange(-avg)%>%
  head(3)


#group by와 summarise 함수
exam %>% group_by(class) %>% summarise(n(),mean(math),sd(math))
exam_clsmath<-exam %>% group_by(class) %>% summarise(count=n(),mean_math=mean(math),sd_math=sd(math))

exam_clshist<-exam%>%group_by(class,gender)%>%summarise(count=n(),mean_hist=mean(history))

exam_clshist<-exam_clshist%>%mutate(prec=count/sum(count))
#prec : 각 반 성비 
exam_clshist<-exam_clshist%>%mutate(prop=count/sum(exam_clshist$count))
#prop: 전체 학생 대비 성 비율

#문제 14
mpg%>%group_by(manufacturer,drv)%>%summarise(cty=mean(cty),hwy=mean(hwy))%>%print(n=100)
#print함수로 결과 출력되는 양을 조절할 수 있음.

#문제 15
mpg%>%group_by(manufacturer)%>%
  filter(class=="suv")%>%
  summarise(mean_sum=mean(sum))%>%
  arrange(-mean_sum)%>%
  head(3)

#문제 16
mpg%>%group_by(trans)%>%
  summarise(mean_displ=mean(displ))%>%
  arrange(-mean_displ)%>%
  head(3)

#문제 17
mpg%>%group_by(manufacturer)%>%
  filter(cyl==4)%>%
  summarise(count=n())%>%
  arrange(-count)%>%
  head(3)



