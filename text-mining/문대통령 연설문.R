#라이브러리 설치
library(multilinguer)
library(KoNLP)
library(dplyr)

install.packages("tidytext")
library(tidytext)

text<-tibble(
  value=c("대한민국은 민주공화국이다.","대한민국의 주권은 국민에게 있고, 모든 권력은 국민으로부터 나온다.")
)

text
extractNoun(text$value)

text %>%
  unnest_tokens(input = value, output=word, token=extractNoun)


#문재인 대통령 연설문 불러오기
raw_moon<-readLines("./text-mining/speech_moon.txt",encoding = "UTF-8")

# 기본적 전처리
library(stringr)

install.packages("textclean")
library(textclean)

moon<-raw_moon %>%
  str_replace_all("[^가-힣]"," ")%>%
  str_squish() %>%
  as_tibble()

# 명사 기준 토큰화
word_noun<-moon %>%
  unnest_tokens(input=value, output=word,token=extractNoun)

word_noun<-word_noun %>%
  count(word,sort=T)%>%
  filter(str_count(word)>1)

top20<-word_noun %>%
  head(20)

library(ggplot2)
ggplot(top20,aes(x=reorder(word,n),y=n))+geom_col()+
  coord_flip()+
  geom_text(aes(label=n),hjust=-0.3)+
  labs(x=NULL)+
  theme(text=element_text(family="NanumGothic"))

# 워드 클라우드 만들기
install.packages("showtext")
library(showtext)

font_add_google(name="Black Han Sans", family = "blackhansans")
showtext_auto()

install.packages("ggwordcloud")
library(ggwordcloud)

ggplot(word_noun, aes(label=word, size=n, col=n))+
  geom_text_wordcloud(seed=1234, family="blackhansans")+
  scale_radius(limits=c(3,NA),range=c(2,15))+
  scale_color_gradient(low="#66aaf2",high="#004E41")+
  theme_minimal()

# 토큰화
sentences_moon<-raw_moon%>%
  str_squish() %>%
  as_tibble() %>%
  unnest_tokens(input = value,output=sentences,token = "sentences")

# 특정 단어가 사용된 문장 추출
sentences_moon %>%
  filter(str_detect(sentences,"국민"))

sentences_moon %>%
  filter(str_detect(sentences,"일자리"))

