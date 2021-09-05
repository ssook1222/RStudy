setwd("../RStudy/Day7")  ## Working directory setting 
list.files() ## file list

file_list<-list.files() ## file list 
file_list

final<-NULL ## 빈 변수 (NULL) 선언 
for(i in 1:length(file_list)){ ## file_list의 1번째방부터 6번째(길이)까지    
  file<-read.csv(file_list[i]) ## file_list의 i번째 방에 있는 csv 파일 read 
  final<-rbind(final,file)  
  cat("\n",i) 
}

dim(final)
head(final)

write.csv(final,file="financedata.csv")
