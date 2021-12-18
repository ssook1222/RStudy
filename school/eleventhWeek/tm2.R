library(multilinguer)
library(KoNLP)
library(tidytext)
library(dplyr)

text <- tibble(value=c("대한민구은 민주공화국이다.","대한민국의 주권은 국민에게 있고,
                       모든 권력은 국민으로부터 나온다."))

#품사 기준 추출
text%>%unnest_tokens(input=value, output = word, token=extractNoun)

#띄어쓰기 기준 추출
text%>%unnest_tokens(input=value, output = word, token="words")

#명사 기준 토큰화
word_noun <- moon %>% unnest_tokens(input=value, output=word,
                                    token=extractNoun)
word_noun #한계가 있음.

#명사 빈도 분석, 2글자 이상만
word_noun <- word_noun %>% count(word, sort = T) %>%
  filter(str_count(word)>1)

word_noun

#상위 20개 단어에 대한 막대 그래프 그리기
#글자체 변경
library(showtext)
font_add_google(name="Black Han Sans", family="BHS")
showtext_auto()

ggplot(top20)

top20 <- word_noun %>% head(20)

ggplot(top20, aes(reorder(word, -n),n,fill=word))+
  geom_bar(stat="identity")+
  geom_text(aes(label=n),hjust=-0.3)+
  labs(title="문재인 출마 연설문 명사 빈도")+
  theme(title=element_text(size=12,family="BHS"))

#워드클라우드 만들기
ggplot(word_noun, aes(label=word,size=n,col=n))+
  geom_text_wordcloud(seed=1234,family="BHS")+ #난수 세팅, 글자 깨짐 조정
  scale_radius(limits=c(2,NA), #최소/최대 단어 : 최소 3번 이상 단어 체크, NA => 제한 X
               range=c(3,15) #최소, 최대 글자 크기 / 최소 빈도수 사이즈는 3, 최대 빈도수 크기는 30
  )+
  scale_color_gradient(low="darkgreen",high="darkred")+theme_minimal()

#문장 기준으로 토큰화
#온점 제거 안 함. .기준으로 문장이 구분되기 때문
sentences_moon <- raw_moon %>% str_squish() %>%
  as_tibble()%>%unnest_tokens(input=value,output=sentence,token = "sentences")

#빈도수가 가장 많은 단어가 포함된 문장 확인
sentences_moon %>% filter(str_detect(sentence,"국민"))%>%print.data.frame(right=F)
sentences_moon %>% filter(str_detect(sentence,"일자리"))%>%print.data.frame(right=F)

#글자가 안 잘리게 출력하려면 %>%print.data.frame(right=F) 사용






