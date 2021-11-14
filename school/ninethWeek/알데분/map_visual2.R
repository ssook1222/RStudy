library(dplyr)

install.packages("stringi")
library(stringi)

install.packages("devtools")
library(devtools)

devtools::install_github("cardiomoon/kormaps2014")
library(kormaps2014)

str(kormap1)
str(korpop1)

korpop1 <- korpop1
korpop1 <- rename(korpop1,pop=총인구_명, name=행정구역별_읍면동)

library(ggiraphExtra)
library(ggplot2)

ggChoropleth(data=korpop1,aes(fill=pop,map_id=code,tooltip=name)
             ,map=kormap1,interactive = T)

new_tbc <- tbc

ggChoropleth(data=new_tbc,aes(fill=NewPts, map_id=code,tooltip=name),map=kormap1,
             interactive = T) #2001년 데이터만 출력

new_tbc_2002 <- new_tbc %>% filter(year==2002)
ggChoropleth(data=new_tbc_2002,aes(fill=NewPts, map_id=code,tooltip=name),
             map=kormap1, interactive = T)





