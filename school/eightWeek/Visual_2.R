library(dplyr)
library(ggplot2)

#막대 그래프 그리기
df_mpg<-mpg%>%group_by(drv)%>%summarise(mean_sum=mean(city+highway))
ggplot(df_mpg,aes(drv,mean_sum))+geom_bar(stat="identity")

#내림차순으로 보기 /reorder 사용
ggplot(df_mpg,aes(reorder(drv,-mean_sum),mean_sum))+geom_bar(stat="identity")

#색 입히기
ggplot(df_mpg,aes(reorder(drv,-mean_sum),mean_sum,fill=drv))+geom_bar(stat="identity")

#빈도 박대 그래프 그리기
  #범주형 말고 계량 척도도 가능
  #qplot 외에 다양한 옵션 적용 가능

#거미줄 그래프는 비추, 가로 그래프는 많이 쓰이니 참고

ggplot(mpg, aes(class))+geom_bar()

#계량척도 적용
ggplot(mpg,aes(highway))+geom_bar()

#클래스에 색을 입히기
ggplot(mpg,aes(class,fill=class))+geom_bar()

#축 범위 제한
ggplot(mpg,aes(class,fill=class))+geom_bar()+xlim(c("compact","midsize","suv"))
#3가지 범위로만 제한됨

#가로 형태로
ggplot(mpg,aes(class,fill=class))+geom_bar()+coord_flip()

#거미줄 그래프 형태로
ggplot(mpg,aes(class,fill=class))+geom_bar()+coord_polar()

ggplot(mpg,aes(class,fill=fuel))+geom_bar()
#dodge를 이용해 옆으로 쌓기, 한 막대에 있는 구분되는 부분 옆으로 놓기
ggplot(mpg,aes(class,fill=fuel))+geom_bar(position = "dodge")

#막대그래프 비교를 위해 크기를 동일하게 조정, 그 안에서의 비중을 보여줌
ggplot(mpg,aes(class,fill=fuel))+geom_bar(position ="fill")
