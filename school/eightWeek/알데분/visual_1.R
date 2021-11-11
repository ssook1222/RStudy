library(dplyr)
library(ggplot2)

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

#데이터 시각화 
ggplot(mpg,aes(displ,highway))+geom_point() + xlim(3,6)+
  ylim(20,30) #181개의 데이터 제거됨.


#점의 색상 변경
#구동방식에 따라 색상 다르게 표현.
#색상 구분 기준변수는 문자나 범주형 척도여야 함.
ggplot(mpg,aes(displ,highway,color=fuel))+geom_point() + 
  xlim(3,6)+ylim(20,30)

#size와 shape -> 점마다 도형과 크기를 적용
ggplot(mpg,aes(displ,highway,color=fuel))+geom_point(aes(shape=drv,size=fuel))
ggplot(mpg,aes(displ,highway,color=drv))+geom_point(aes(shape=drv,size=fuel))

#도형만 적용
ggplot(mpg,aes(displ,highway,color=drv))+geom_point(aes(shape=drv))

ggplot(mpg,aes(city,highway))+geom_point()
ggplot(mpg,aes(city,highway))+geom_point()+xlim(0,30)+ylim(0,40)
ggplot(mpg,aes(city,highway,color=cyl))+geom_point()+xlim(0,30)+ylim(0,40)
#좌하향일수록 색이 옅음.

ggplot(mpg,aes(city,highway,color=cyl))+geom_point(aes(shape=drv))+
  xlim(0,30)+ylim(0,40)
#위로 갈수록 절륜구동차가 많음.
ggplot(mpg,aes(city,highway,color=cyl))+geom_point(aes(shape=drv))+
  xlim(0,30)+ylim(0,40)+geom_smooth()
#선을 이용해 추세선을 확인 가능

midwest <- ggplot2::midwest 
ggplot(midwest,aes(poptotal, popasian))+geom_point()
ggplot(midwest,aes(poptotal, popasian))+geom_point()+xlim(0,350000)+ylim(0,5000)





