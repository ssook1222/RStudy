install.packages("plotly")
library(plotly)

#데이터 전처리
mpg<-mpg
mpg<-rename(mpg,fuel=fl,city=cty,highway=hwy)
mpg$drv<-ifelse(mpg$drv=="f","forward",mpg$drv)
mpg$drv<-ifelse(mpg$drv=="r","rear",mpg$drv)
mpg$drv<-ifelse(mpg$drv=="f","forward",ifelse(mpg$drv=="r","rear",mpg$drv))
mpg$fuel<-ifelse(mpg$fuel=="c","CNG",
                 ifelse(mpg$fuel=="e","ethanol",
                        ifelse(mpg$fuel=="d","diesel",
                               ifelse(mpg$fuel=="p","premium",
                                      ifelse(mpg$fuel=="r","regular",mpg$fuel)))))
mpg$fuel<-ifelse(mpg$fuel=="e","ethanol",mpg$fuel)
mpg$fuel<-ifelse(mpg$fuel=="d","diesel",mpg$fuel)
mpg$fuel<-ifelse(mpg$fuel=="p","premium",mpg$fuel)
mpg$fuel<-ifelse(mpg$fuel=="r","regular",mpg$fuel)

#그래프 그리기
p1 <- ggplot(data=mpg,aes(displ,highway,col=drv))+geom_point()
ggplotly(p1)

p2 <- ggplot(mpg,aes(class,fill=class))+geom_bar()+coord_flip()
ggplotly(p2)

p3 <- ggplot(mpg,aes(class,fill=fuel))+geom_bar(position = "dodge")+coord_flip()
ggplotly(p3)

#인터랙티브 막대그래프 그리기
diamonds <- as.data.frame(diamonds)
head(diamonds)
str(diamonds)
p4 <- ggplot(data=diamonds,aes(cut,fill=clarity))+geom_bar(position = "dodge")
ggplotly(p4)

p5 <- ggplot(data=diamonds,aes(cut,fill=color))+geom_bar(position = "dodge")
ggplotly(p5)

#인터랙티브 시계열 그래프
install.packages("dygraphs")
library(dygraphs)

install.packages("xts")
library(xts)

library(ggplot2)
economics <- ggplot2::economics
eco <- xts(economics$unemploy,order.by=economics$date)
dygraph(eco)%>%dyRangeSelector()

#복수의 인터랙티브 그래프 한 번에 그리기
eco_a <- xts(corona19$total_cases,order.by = corona19$date)
eco_b <- xts(corona19$total_vaccinations/100,order.by = corona19$date) #scale을 맞춰줌

eco_c <- cbind(eco_a,eco_b)

colnames(eco_c) <- c("total_cases","total_vaccinations")
dygraph(eco_c)%>%dyRangeSelector()




