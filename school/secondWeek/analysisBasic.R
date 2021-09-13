install.packages("readxl")
library(readxl)

install.packages("writexl")
library(writexl)

View(exam)
exam<-read.csv("./school/data/exam.csv", fileEncoding = "EUC-KR")
exam_addition <- read_excel("./school/data/exam_addition.xlsx",sheet = 1)

#불필요한 변수 제거
exam_addition$id<-NULL
exam$id<-NULL

# 데이터 프레임 내보내기
write.csv(exam_addition,file="week2first.csv")
write_xlsx(exam_addition,path="./school/data/week2second.xlsx")

# 조건, 변수 연산자
table(exam$address=="원효로")
table(exam$gender!="Female") #table(exam$gender=="Male")

table(exam$math==50)
table(exam$math!=50)
table(exam$math<=50)
table(exam$math>=50)

table(exam$english<=50&exam$history>=80)
table(exam$math>=90|exam$history>=90)
table(exam$address=="효창동"|exam$address=="청파동"|exam$address=="서계동")
table(exam$address%in%c("효창동","청파동","서계동"))

# 변수명 바꾸기
library(ggplot2)
library(dplyr)

head(mpg)
mpg<-mpg

str(mpg)
summary(mpg)
table(mpg$drv)
table(mpg$fl)
table(mpg$class)

mpg<-rename(mpg,fuel=fl,city=cty,highway=hwy)

# 측정 값 바꾸기
mpg$drv<-ifelse(mpg$drv=="f","forward",mpg$drv)
mpg$drv<-ifelse(mpg$drv=="r","rear",mpg$drv)

# 한 줄로 변경
mpg$drv<-ifelse(mpg$drv=="f","forward",ifelse(mpg$drv=="r","rear",mpg$drv))

# 측정 값 바꾸기2
mpg$fuel<-ifelse(mpg$fuel=="c","CNG",
                 ifelse(mpg$fuel=="e","ethanol",
                        ifelse(mpg$fuel=="d","diesel",
                               ifelse(mpg$fuel=="p","premium",
                                      ifelse(mpg$fuel=="r","regular",mpg$fuel)))))

mpg$fuel<-ifelse(mpg$fuel=="e","ethanol",mpg$fuel)
mpg$fuel<-ifelse(mpg$fuel=="d","diesel",mpg$fuel)
mpg$fuel<-ifelse(mpg$fuel=="p","premium",mpg$fuel)
mpg$fuel<-ifelse(mpg$fuel=="r","regular",mpg$fuel)
