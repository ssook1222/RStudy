#라이브러리 추가
install.packages("dplyr")
library(dplyr)

#데이터 추가
corona<-read.csv("./Day5/seoul_corona.csv", fileEncoding = "euc-kr", quote="")
corona

# 변수 이름 변경
corona <- rename(corona,date=X.확진일.)
corona <- rename(corona,locate=X.지역.)

# 사용할 열만 추출
corona %>% select(date,locate)

corona <- corona %>%
  group_by(locate) %>%
  summarise(n = n())

write.csv(corona,file="corona.csv")
