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

#회사별 상위 5개 도심 연비 평균을 구하기 위한 전처리 진행
mpg_suv <- mpg %>% group_by(manufacturer)%>%filter(class=="suv")%>%
  summarise(mean_city=mean(city))%>%
  arrange(-mean_city)%>%head(5)

ggplot(mpg_suv, aes(reorder(manufacturer,mean_city),mean_city,
                    fill=manufacturer))+geom_bar(stat="identity")+coord_flip()

ggplot(mpg_suv, aes(reorder(manufacturer,mean_city),mean_city,
                    fill=manufacturer))+geom_bar(stat="identity")+coord_flip()+
  labs(title="회사별 suv 도심연비 평균 비교",x="제조사",y="suv 도심연비 평균")+
  theme(text=element_text(size = 12,family = "NanumGothic"))

#히스토그램 geom_histogram을 이용해 그리기
#계량형 척도로 측정된 변수에 대해 구간별 빈도를 구함
#binwidth => 디폴트가 1, 구간을 어떻게 쪼갤건지 정함

#여러 개 막대에 대해 색상을 구분하려면 명령문이 복잡해짐
ggplot(mpg, aes(highway))+geom_histogram()
ggplot(mpg, aes(highway))+geom_histogram(binwidth=0.5) #막대 개수는 동일
ggplot(mpg, aes(highway))+geom_histogram(binwidth=5)

#색상 채우기
ggplot(mpg, aes(highway))+geom_histogram(binwidth = 1, fill="whitesmoke", color="black")+
  labs(title="고속도로 연비 히스토그램",x="고속도로 연비",y="빈도")+
  theme(text=element_text(size = 12,family = "NanumGothic"))
#color => 막대를 쌓는 것

colors() #적용가능한 색상








