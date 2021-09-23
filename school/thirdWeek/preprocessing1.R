exam<-read.csv("./school/data/exam.csv",fileEncoding ="euc-kr",stringsAsFactors = F)

library(dplyr)

#1반 학생만 추출
exam_1<-exam%>%filter(class==1)

#변수를 삭제하는 명령어
rm(exam_1)

#남학생들의 영어시험 분산 구하기
exam_male<-exam%>%filter(gender=="Male")
var(exam_male$english)

#filter함수
#문제 1
exam_new<-exam%>%filter(class==1|class==2|class==3)
exam_new<-exam%>%filter(class %in% c(1,2,3))
mean(exam_new$math)

#문제 2
exam_N4<-exam%>%filter(class!=4&math>=90|class!=4&math>=95)
exam_N4<-exam%>%filter(class!=4)%>%filter(math>=90|history>=90)

#문제 3 상위 10%인 학생의 영어 점수 출력
exam%>%filter(english>=quantile(english, probs=c(0.9)))

  #상위 10%의 기준 값은?  
  quantile(exam$english,probs = c(0.9))

  #한 번 파이브 연산자로 데이터프레임을 연결하고 나면 데이터프레임을 
  #반복해 사용해야 될 필요가 없음.
  #데이터프레임 없이 변수명만 작성해도 됨
  
#문제 4
mpg_d4<-mpg%>%filter(displ<=4)  
mpg_d5<-mpg%>%filter(displ>=5)
mean(mpg_d4$hwy)
mean(mpg_d5$hwy)

#문제 5
audi<-mpg%>%filter(manufacturer=="audi")
toyota<-mpg%>%filter(manufacturer=="toyota")

mean(audi$cty)
mean(toyota$cty)

#문제 6
mpg$manufacturer
mpg_three<-mpg%>%filter(manufacturer=="chevrolet"|manufacturer=="ford"|manufacturer=="honda")
mean(mpg_three$hwy)



