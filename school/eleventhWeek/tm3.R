raw_moon <- readLines("./school/data/moon_speech.txt",encoding = "euc-kr")
moon <- raw_moon %>% as_tibble() %>% mutate(president="moon")
raw_park <- readLines("./school/data/speech_park.txt",encoding = "euc-kr")
park <- raw_park %>% as_tibble() %>% mutate(president="park")

bind_speeches <- bind_rows(moon, park) %>% relocate(
  president, .before = value
)

#특수문자 제거 및 연속된 공백 삭제
speeches <- bind_speeches%>%mutate(value=str_replace_all(value, "[^가-힣]"," "),
                                   value=str_squish(value)
                                   )
#명사 기준 토큰화
library(tidytext)
library(KoNLP)

speeches <- speeches %>% unnest_tokens(input=value, output = word,
                                       token = extractNoun) #문장에서 명사로 변경되어 늘어남.

#대통령별 단어 빈도수 구하기
frequency <-speeches %>% count(president, word) %>% #president로 묶은 다음, word로 묶는 빈도수 구하기
  filter(str_count(word)>1) #내림차순 정렬이 아님

#slice_max 함수 활용해 빈도 높은 상위 10개 사례 추출
#arrange와 head를 한 번에 합쳐서 진행

top10 <- frequency %>% group_by(president)%>%slice_max(n, n=10)%>% #앞은 n은 변수, 뒤는 개수
  print(n=Inf) #모든 행을 출력해라. (동률이 있음)

#동률 제외
top10 <- frequency %>% group_by(president)%>%slice_max(n, n=10,with_ties = F)%>% #앞은 n은 변수, 뒤는 개수
  print(n=Inf) #단점은 앞에 있는 단어에서 잘림

frequency %>% group_by(president)%>%slice_max(n, n=10,with_ties = F)

#빈도수 상위 10개에 대한 막대 그래프 만들기
ggplot(top10, aes(reorder(word,n),n,fill=president))+ #오름차순, 대통령별로 색상 다르게
  geom_bar(stat="identity")+
  coord_flip()+ #x,y축 교환
  facet_wrap(~president) #대통령별로 그리겠다는 의미

#x축 설정을 president 측정 값에 따라 구분하기
ggplot(top10, aes(reorder(word, n),n,fill=president))+
  geom_bar(stat="identity")+
  coord_flip()+ #x,y축 교환
  facet_wrap(~president,scales="free_y")

#지나치게 많이 쓰인 특정 단어 제외
top10 <- frequency %>% filter(word!="국민")%>%group_by(president)%>%
  slice_max(n,n=10, with_ties=F)%>%print(n=Inf)

ggplot(top10, aes(reorder(word,n),n,fill=president))+ #n 즉, 대통령 구분 없이 전체 빈도에 대해 정렬
  geom_bar(stat="identity")+coord_flip()+
  facet_wrap(~president,scales="free_y")

#막대 그래프 빈도수 정렬 오름차순으로 정렬(구분하여 다시 만들기)
ggplot(top10, aes(reorder_within(word,n,president),n,fill=president))+
  geom_bar(stat="identity")+coord_flip()+
  facet_wrap(~president,scales="free_y")

#x축 항목(word) 명칭 조정
ggplot(top10, aes(reorder_within(word,n,president),n,fill=president))+
  geom_bar(stat="identity")+coord_flip()+
  facet_wrap(~president,scales="free_y")+
  scale_x_reordered()+
  labs(x=NULL)
  
  
  





