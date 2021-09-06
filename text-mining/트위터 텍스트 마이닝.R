#koNLP 설치
install.packages("multilinguer")
library(multilinguer)
install_jdk()

install.packages(c("stringr", "hash", "tau", "Sejong", "RSQLite", "devtools"), type = "binary")

install.packages("remotes")
remotes::install_github("haven-jeon/KoNLP", 
                        upgrade = "never",
                        INSTALL_opts=c("--no-multiarch"))

library(KoNLP)
library(dplyr)
library(stringr)
install.packages("extrafont")
library(extrafont)
font_import()+options(warn = 0)
y

# 데이터 로드
twitter<-read.csv("./text-mining/twitter.csv", header = T, stringsAsFactors = F, fileEncoding = "UTF-8")

# 변수명 수정
twitter<-rename(twitter, no=번호, id=계정이름, date=작성일, tw=내용)

# 특수문자 제거
twitter$tw<-str_replace_all(twitter$tw, "\\W", " ")
head(twitter$tw)

# 단어 빈도표 만들기
nouns<-extractNoun(twitter$tw)
wordcount<-table(unlist(nouns))
df_word<-as.data.frame(wordcount, stringsAsFactors = F)

df_word<-rename(df_word, word=Var1, freq=Freq)

# 단어 추출
df_word<-filter(df_word, nchar(word) >=2)

top20 <- df_word %>%
  arrange(desc(freq)) %>%
  head(20)

top20

# 단어 빈도 막대 그래프 만들기

library(ggplot2)

# 빈도 순서 변수 생성 
order<-arrange(top20, freq)$word
ggplot(data = top20, aes(x=word, y=freq))+
  ylim(0,2500)+geom_col()+coord_flip()+
  scale_x_discrete(limit=order)+geom_text(aes(label=freq),hjust=-0.3)+theme(text=element_text(size=12, family="NanumGothic"))

# 단어 워드클라우드 생성

library(wordcloud)

wordcloud(words=df_word$word, freq=df_word$freq,
          min.freq = 2, max.words = 200,random.order = F,rot.per = .1,
          scale=c(4,0.3),colors=pal, family = "NanumGothic" )



  
  