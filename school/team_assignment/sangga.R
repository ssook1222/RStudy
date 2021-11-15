#서울시 지도를 만들어 보자
install.packages("ggmap")
install.packages("raster")
install.packages("rgeos")
install.packages("maptools")
install.packages("rgdal")
install.packages("doBy")

library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(dplyr)
library(doBy)

sangga<-read.csv("./school/team_assignment/visualization/data/sangga.csv",stringsAsFactors = F)
seoul_id<-read.csv("./school/team_assignment/visualization/data/seoul_id.csv",stringsAsFactors = F)
sangga_new<-left_join(sangga,seoul_id,by='시군구명')
map<-shapefile("./school/team_assignment/visualization/data/TL_SCCO_SIG.shp")

map<-spTransform(map, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
new_map <- fortify(map, region = 'SIG_CD')

new_map$id <- as.numeric(new_map$id)
seoul_map <- new_map[new_map$id <= 11740,]

sangga_real<-sangga_new[, c(5,11,15,38,39,40)]


sangga_test<-sangga_real[sample(nrow(sangga_real),120), ]
sangga_test_new <- sampleBy(~1, frac=.01, data=sangga_real, systematic=TRUE)


sangga_merge <- left_join(seoul_map, sangga_test_new, by='id')
str(sangga_merge)

#지역구별 표준산업분류명
sangga_merge$상권업종대분류명 <- as.factor(sangga_merge$상권업종대분류명)

ggplot() + labs(title="서울 지역구별 가장 많은 상권업종")+
  geom_polygon(data=sangga_merge, aes(x=long, y=lat, group=group, fill = 상권업종대분류명), color='white')+
  theme(text=element_text(size = 10,family = "NanumGothic"),
        plot.title=element_text(size = 14,family = "NanumGothic",face="bold",hjust = 0.5))


# 지역구 별 가게 개수
sangga_test2<- sangga_test_new%>%group_by(시군구명)%>%summarise(count=n())
seoul_id <- seoul_id%>%arrange(시군구명)

sangga_test2 <- cbind(sangga_test2,seoul_id)
sangga_test2<-sangga_test2[, c(2:4)]
sangga_merge2 <- left_join(seoul_map, sangga_test2, by='id')

ggplot()+
  labs(title="서울 지역구별 상가 수")+
  geom_polygon(data=sangga_merge2, aes(x=long, y=lat, group=group, fill = count), color='steelblue4')+
  theme(text=element_text(size = 10,family = "NanumGothic"),
        plot.title=element_text(size = 15,family = "NanumGothic",face="bold",hjust = 0.5))
