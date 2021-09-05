
#패키지 설치
install.packages("rJava")
rJava::.jinit()

install.packages("multilinguer")
multilinguer::install_jdk()

install.packages("KoNLP", 
                 repos = c("https://forkonlp.r-universe.dev",
                           "https://cloud.r-project.org"),
                 INSTALL_opts = c("--no-multiarch")
)

library("KoNLP")
library("dplyr")

install.packages("extrafont")
library(extrafont)
font_import()
y

# 한글 폰트 넣기


# 단어사전 넣기
buildDictionary(ext_dic = "woorimalsam")
useNIADic()
useSejongDic()

# 데이터 불러오기
txt <- readLines("./text-mining/hiphop.txt")
head(txt)

# 특문 제거
install.packages("stringr")
library(stringr)
txt<-str_replace_all(txt,"\\W", " ")

# 가사에서 명사 추출
nouns<-extractNoun(txt)

# 명사 리스트를 문자열 벡터로 변환, 단어별 빈도표 생성
wordcount<-table(unlist((nouns)))

# 데이터 프레임으로 변환
df_word<-as.data.frame(wordcount, stringsAsFactors = F)

# 변수명 수정
df_word<-rename(df_word, word=Var1,freq=Freq)

# 자주 사용한 단어 빈도표 만들기
df_word<-filter(df_word, nchar(word)>=2)

# 상위 단어 20개 추출
top_20<-df_word%>%
  arrange(desc(freq))%>%
  head(20)

top_20

# 워드 클라우드로 시각화하기
install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)

# 8개 색상 추출
pal <- brewer.pal(8,"Dark2")

# 난수 고정하기
set.seed(1234)

# 워드 클라우드 만들기
wordcloud(words=df_word$word, freq=df_word$freq,
          min.freq = 2, max.words = 200,random.order = F,rot.per = .1,
          scale=c(4,0.3),colors=pal, family = "NanumGothic" )
