#단계 구분도 : choropleth map
#지도 상 동계치 결과에 따라 음영/색상/패턴별 차이를 다르게 보여주는 지도
library(dplyr)
library(tibble)
crime <- USArrests
crime <- rownames_to_column(crime, var="state") #변수명이 없는 것에 변수명 부여
crime$state <- tolower(crime$state) #주 명칭을 소문자로 변경

#지도 불러오기
install.packages("maps")
library(maps)

#map의 state(위도, 경도, 지역 제반 정보)를 데이터 프레임 형태로 저장
library(ggplot2)
states_map <- map_data("state") #map에 내장된 state

#단계구분도 생성 : ggChoropleth
install.packages("mapproj")
library(mapproj)
install.packages("ggiraphExtra")
library(ggiraphExtra)

ggChoropleth(data=crime, aes(fill=Murder, map_id=state),map=states_map)
#Murder 측정결과를 색상으로 구분. aes 활영하려면 ggplot2 불러와야 함.

ggChoropleth(data=crime, aes(fill=Assault,map_id=state),map=states_map,interactive = T)
#마우스를 누르면 왔다갔다 하면서 정보를 볼 수 있음.
#이를 export를 통해 html로 내보낼 수 있음.



