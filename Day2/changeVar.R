df_raw<-data.frame(var1 = c(1,2,1), var2 = c(2,3,2))
df_raw

#dplyr 설치
install.packages("dplyr")
#dplyr 로드
library(dplyr)

df_new<-df_raw
df_new

#var2를 v2로 수정
df_new<-rename(df_new,v2=var2)
df_new

#변수 변경 비교
df_raw
df_new
