library(dplyr)
library(stringr)

raw_moon <- readLines("./school/data/moon_speech.txt",encoding = "euc-kr")
head(raw_moon)

#불필요한 문자 제거하기
moon <- str_replace_all(string=raw_moon,pattern="[^가-힣]",replacement = " " ) #옵션 제거 가능

#연속된 공백 제거하기
moon <- moon %>% str_squish()

#tibble 형태로 바꿔주기
#토큰화하려면 tibble 형태로 만들기
moon <- as_tibble(moon)

#토큰화
install.packages("tidytext")
library(tidytext)

#unnest_tokens / token : words(단어), sentense(문장), characters(글자)
#단어 형태로 토큰 나눈 걸 티블에 저장
word_space <- moon%>% unnest_tokens(input = value,
                                    output=word,
                                    token="words")

#단어 빈도 측정, count 사용
word_space <- word_space %>% count(word, sort=T)

#한 글자 단어는 의미 파악이 어려우므로 처리해야 됨.
word_space <- word_space %>% filter(str_count(word)>1)

#상위 20개의 단어만 시각화
library(ggplot2)

#글자 깨지기 방지용으로 기본 세팅 진행  
theme_set(theme_grey(base_family='NanumGothic'))

top20 <- word_space %>% head(20)
ggplot(top20, aes(reorder(word, -n),n,fill=word))+
  geom_bar(stat="identity")+
  geom_text(aes(label=n),hjust=-0.3)+
  labs(title="문재인 출마 연설문 단어 빈도")+
  theme(title=element_text(size=12))

#워드 클라우드 만들기
install.packages("ggwordcloud")
library(ggwordcloud)

ggplot(word_space, aes(label=word,size=n))+
  geom_text_wordcloud(seed=1234,family="NanumGothic")+ #난수 세팅, 글자 깨짐 조정
  scale_radius(limits=c(3,NA), #최소/최대 단어 : 최소 3번 이상 단어 체크, NA => 제한 X
               range=c(3,30) #최소, 최대 글자 크기 / 최소 빈도수 사이즈는 3, 최대 빈도수 크기는 30
               )

#워드 클라우드 색상 조정
ggplot(word_space, aes(label=word,size=n,col=n))+
  geom_text_wordcloud(seed=1234,family="NanumGothic")+ #난수 세팅, 글자 깨짐 조정
  scale_radius(limits=c(3,NA), #최소/최대 단어 : 최소 3번 이상 단어 체크, NA => 제한 X
               range=c(3,30) #최소, 최대 글자 크기 / 최소 빈도수 사이즈는 3, 최대 빈도수 크기는 30
  )+
  scale_color_gradient(low="#66aaf2",high="#004EA1")+theme_minimal()

  
#글자체 바꾸기
install.packages("showtext")
library(showtext)

font_add_google(name="Noto Sans KR",family="noto")
showtext_auto()

#워드 클라우드 글자 체 변경 후 
ggplot(word_space, aes(label=word,size=n,col=n))+
  geom_text_wordcloud(seed=1234,family="noto")+ #난수 세팅, 글자 깨짐 조정
  scale_radius(limits=c(3,NA), #최소/최대 단어 : 최소 3번 이상 단어 체크, NA => 제한 X
               range=c(3,30) #최소, 최대 글자 크기 / 최소 빈도수 사이즈는 3, 최대 빈도수 크기는 30
  )+
  scale_color_gradient(low="#66aaf2",high="#004EA1")+theme_minimal()









