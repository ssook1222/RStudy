#특정 텍스트에서는 많이 쓰이지만, 다른 텍스트에서 덜 쓰이는
#상대적 빈도가 높은 단어가 무엇인지 파악하는 것이 중요

#Long form 형태 : 아래로 쭉쭉 보는 형태
#Wide form 형태 : 가로로 쭉쭉 보는 형태

df_long <- frequency %>% group_by(president)%>%
  slice_max(n,n=10) %>% 
  filter(word %in% c("국민","우리","정치","행복"))
# 4단어 대통령별로 사용 빈도수 체킹

#long form을 wide form으로 바꾸기
#names_from: 새로운 변수의 이름을 기존의 어디서 가져오지?
#values_from : 변수에 채워 넣을 측정값을 기존의 어디서 가져오지?

library(tidyr)

df_wide <- df_long %>% pivot_wider(
  names_from=president, #새로 만들 데이터 프레임 열
  values_from=n, #새로 만들 데이터 프레임 값 
  values_fill=list(n=0) #NA는 0으로 대치
)

df_long
df_wide

frequency_wide <- frequency %>%
  pivot_wider(names_from = president,
              values_from=n,
              values_fill = list(n=0))

frequency_wide #단어 기준으로 변경

#오즈비 구하기
#A조건 발생 확률이 B조건에서 발생할 확률에 비해 얼마나 더 클지 나타낸 값
#단어가 두 텍스트 중 어디 등장할 확률이 높은지, 즉 단어의 상대적인 중요도를 알 수 있음

#단어 비중 구하기
#연설문 별로 각 단어의 빈도를 모든 단어의 빈도 합으로 나눔
#단어 빈도가 0이면 오즈비 구할 떄 문제(divides zero)가 발생해서
#분모와 분자에 각각 1을 더함

frequency_wide <- frequency_wide %>%
  mutate(ratio_moon=((moon+1)/(sum(moon+1))),
         ratio_park=((park+1)/(sum(park+1))))

#오즈비 구하기
#한 텍스트의 단어 비중을 다른 텍스트의 단어 비중으로 나눔

frequency_wide <- frequency_wide %>%
  mutate(odds_ratio=ratio_moon/ratio_park)
#오즈비가 1보다 크다면 분자에 올라간 텍스트에 더 많이 사용되었다는 의미

frequency_wide%>%arrange(-odds_ratio)
frequency_wide%>%arrange(odds_ratio)
frequency_wide%>%arrange(abs(1-odds_ratio))

#오즈비가 가장 높은 10개 단어와 가장 낮은 10개 단어 추출하여 top10 만들기
top10 <- frequency_wide %>% filter(rank(odds_ratio)<=10|rank(-odds_ratio)<=10)%>%
  arrange(-odds_ratio) #값이 작은 순서 / 큰 순서대로

top10

#막대 그래프 그리기
top10 <- top10 %>% mutate(president=ifelse(odds_ratio>1,"moon","park"),
                          n=ifelse(odds_ratio>1,moon,park)) 

library(tidytext)

#상대적으로 많이 사용된 단어 빈도 막대그래프
ggplot(top10, aes(x=reorder_within(word, n, president),n,fill=president))+
  geom_bar(stat="identity")+coord_flip()+
  facet_wrap(~president, scales="free_y")+
  scale_x_reordered()+labs(x=NULL)

#그래프 별로 축 설정하기
ggplot(top10, aes(x=reorder_within(word,n,president),n,fill=president))+
  geom_bar(stat="identity")+coord_flip()+
  facet_wrap(~president, scales="free")+
  scale_x_reordered()+labs(x=NULL) #막대 길이가 같아도 빈도가 다름에 주의.
  #축을 그래프에 따라 맞춰 주었기 때문

#주요 단어가 사용된 문장 살펴보기

#문장 단위로 토큰화하기
speeches_sentence <- bind_speeches %>% as_tibble() %>%
  unnest_tokens(input = value, output = sentence, token="sentences")

#주요 단어가 사용된 문장 추출하기
speeches_sentence %>% filter(president=="moon"&str_detect(sentence,"복지국가"))
speeches_sentence %>% filter(president=="park"&str_detect(sentence,"행복"))


#중요도 살펴보기
frequency_wide %>% arrange(abs(1-odds_ratio)) %>% head(10) #보편적인 단어

#중요도가 비슷하고 빈도가 높은 단어 살펴보기
#각 연설문에서 빈도수가 5회 이상이면서 중요도가 비슷한 단어 추출
#두 연설문에서 모두 강조한 단어 확인
frequency_wide %>% filter(moon>=5&park>=5)%>%
  arrange(abs(1-odds_ratio))%>%head(10)







































