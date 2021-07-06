#데이터 시각화 패키지 설치
install.packages("ggplot2")

#ggplot2 라이브러리 import
library(ggplot2)

#mpg 데이터 사용하여 시각화

#1. 막대 그래프 형태
qplot(data=mpg,x=hwy)

#2. 선 그래프 형태
qplot(data=mpg,x=drv,y=hwy,geom="line")

#3. box plot 형태
qplot(data=mpg, x=drv, y=hwy, geom="boxplot", colour=drv)
