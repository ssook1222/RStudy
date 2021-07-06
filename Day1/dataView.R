exam<-read.csv("./Day1/csv_exam.csv")
exam

#문자가 있는 파일을 불러올 때는 stringsAsFactors 사용
#df_csv_exam <-read.csv("./Day1/csv_exam.csv", stringsAsFactors = F)
#df_csv_exam

#첫 행이 변수명이 아닌 데이터인 경우
#df_csv_exam <-read.csv("./Day1/csv_exam.csv", col_names= F)

#데이터 앞부분 확인하기 cf) 뒤는 tail(변수, 보여줄 부분)
head(exam)

#출력할 데이터 앞부분 설정
head(exam,10)

#뷰어 창에서 확인
View(exam)

#몇 행, 몇 열로 구성되어 있는가?
dim(exam)

#데이터 속성 파악하기
str(exam)

#요약 통계량 산출하기
summary(exam)


