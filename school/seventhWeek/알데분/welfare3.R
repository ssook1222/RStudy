library(dplyr)

#문제1. 데이터 프레임에 있는 교육수준 변수 명칭을 바꾼 후, 빈도수를 확인하시오.
welfare_test <- welfare
welfare_test<-welfare%>%rename(
  education=h10_g6
)

table(welfare_test$education)

#문제2. education 측정 값은 아래와 같은데, 1(미취학)을 NA로 변경하고 NA 개수를 구하시오.
welfare_test$education <- ifelse(welfare_test$education==1,NA,welfare_test$education)

#문제3. welfare에 있는 education 변수를 welfare7에 복사하시오.
welfare7$education <- welfare_test$education

#문제4. education과 education_name 두 변수로 구성된 list_education 데이터 프레임을 만드시오.
list_education <- data.frame(education=c(2:9),education_name=c("무학",
                                                            "초졸",
                                                            "중졸",
                                                            "고졸",
                                                            "전문대졸",
                                                            "대졸",
                                                            "석사",
                                                            "박사"
                                                            ))

#문제5: welfare7과 list_education을 통합하고, education과 education_name의 척도를 범주형으로 
#통합하시오.
str(welfare7$education)
str(list_education$education)

welfare7$education<-as.factor(welfare7$education)
list_education$education<-as.factor(list_education$education)

welfare7<-left_join(welfare7,list_education,by="education")

welfare7$education_name<-as.factor(welfare7$education_name)

#문제6:education에서 NA를 제외하고 표의 기준에 따라 범주형 척도 변수
#edugrade를 만든 후 빈도수를 확인하시오.

welfare7<-welfare7%>%
  filter(!is.na(education))%>%
  mutate(edugrade=ifelse(education==2|education==3|education==4,"low",
                         ifelse(education==5|education==6,"middle",
                                ifelse(education==7|education==8,"high",
                                       "very high"))))

welfare7<-welfare7%>%
  filter(!is.na(education))%>%
  mutate(edugrade=ifelse(education%in%c(2:4),"low",
                         ifelse(education%in%c(5:6),"middle",
                                ifelse(education%in%c(7:8),"high",
                                       "very high"))))

table(welfare7$edugrade)

#문제7:region_name과 edugrade에 따라 집단을 구분하고 집단별 빈도수를 검토하시오.
welfare7%>%group_by(region_name,edugrade)%>%
  summarise(count=n())%>%
  arrange(-count)

#문제8:region_name과 edugrade에 따라 집단을 구분하고,
#집단별 비율을 1)집단내에서의 비율, 2)전체 사례 대비 비율로 구분하여 구하시오.

#집단내의 비율
welfare7%>%group_by(region_name,edugrade)%>%
  summarise(count=n())%>%
  mutate(rate=count/sum(count))

welfare7%>%group_by(region_name,edugrade)%>%
  summarise(count=n())%>%
  mutate(rate=count/sum(count))

#전체 사례 대비 비율
welfare7%>%group_by(region_name,edugrade)%>%
  summarise(count=n(), rate=count/nrow(welfare7))







