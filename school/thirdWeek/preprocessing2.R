exam<-read.csv("./school/data/exam.csv",fileEncoding ="euc-kr",stringsAsFactors = F)

library(dplyr)

#select 함수 실습
exam_3<-exam %>% select(class,math,english)
exam_minus_address<-exam%>% select(-address)

#특정 문자가 있는지 확인하는 함수 실습
exam%>%select(contains("add")) #add 문자가 들어있는 열만 추출

#분제 7
exam %>%
  filter(class==1) %>% select(gender,math)
  
#문제 8
mpg_cc<-mpg%>%select(class,cty)

#문제 9
mpg_suv<-mpg_cc%>%filter(class=='suv')
mpg_compact<-mpg_cc%>%filter(class=='compact')

mean(mpg_suv$cty)
mean(mpg_compact$cty)

#arrange 함수 실습

#오름차순 정렬
exam %>% arrange(math)
#내림차순 정렬
exam %>% arrange(-math)
exam %>% arrange(desc(math))

#두 개의 기준으로 정렬
exam%>%arrange(class,-math)

#문제 10
mpg%>%filter(manufacturer=="audi")%>%arrange(-hwy)%>%head(3)

