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
ggplot(mpg,aes(displ,highway))+geom_point()

