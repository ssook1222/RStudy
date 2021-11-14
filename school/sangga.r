#서울시 지도를 만들어 보자
install.packages("ggmap")
install.packages("raster")
install.packages("rgeos")
install.packages("maptools")
install.packages("rgdal")

library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(dplyr)

sangga<-read.csv("./school/team_assignment/visualization/data/sangga.csv",stringsAsFactors = F)
seoul_id<-read.csv("./school/team_assignment/visualization/data/seoul_id.csv",stringsAsFactors = F)
sangga_new<-left_join(sangga,seoul_id,by='시군구명')
map<-shapefile("./school/team_assignment/visualization/data/TL_SCCO_SIG.shp")

map<-spTransform(map, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
new_map <- fortify(map, region = 'SIG_CD')

new_map$id <- as.numeric(new_map$id)
seoul_map <- new_map[new_map$id <= 11740,]

sangga_real<-sangga_new[, c(5,11,38,39,40)]


sangga_test<-sangga_real[c(1:120), ]
sangga_merge <- left_join(seoul_map, sangga_test, by='id')
str(sangga_merge)
sangga_merge$표준산업분류명 <- as.factor(sangga_merge$표준산업분류명)
ggplot() + geom_polygon(data=sangga_merge, aes(x=long, y=lat, group=group), fill = 'white', color='black')
ggplot() + geom_polygon(data=sangga_merge, aes(x=long, y=lat, group=group, fill = 상권업종대분류명), color='black')+
  theme(text=element_text(size = 12,family = "NanumGothic"))


