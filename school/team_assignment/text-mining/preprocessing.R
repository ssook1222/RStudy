library(tidytext)
library(KoNLP)
library(dplyr)
library(tidyr)

rda_raw<-read.csv("./school/team_assignment/text-mining/data/rda_talk.csv",stringsAsFactors = F)
np_raw <- read.csv("./school/team_assignment/text-mining/data/np_talk.csv",stringsAsFactors = F)

#불필요한 문자 제거
rda <- str_replace_all(string=rda_raw$Message,pattern="[^가-힣]",replacement = " " )
np <- str_replace_all(string=np_raw$Message,pattern="[^가-힣]",replacement = " " )

#연속된 공백 제거 
rda <- rda %>% str_squish()
np <- np %>% str_squish()

#2개 다 티블 형태로 바꿔주기
rda <- as_tibble(rda)%>% mutate(class="rda")
np <- as_tibble(np)%>% mutate(class="np")

#불용어 제거
rda$value<- gsub("들어왔습니다", "", rda$value)
rda$value<- gsub("나갔습니다", "", rda$value)
rda$value<- gsub("넵", "", rda$value)
rda$value<- gsub("안녕하세요", "", rda$value)
rda$value<- gsub("네", "", rda$value)
rda$value<- gsub("교수님", "", rda$value)
rda$value<- gsub("님", "", rda$value)
rda$value<- gsub("아 ", "", rda$value)
rda$value<- gsub("하 ", "", rda$value)
rda$value<- gsub("감사합니다", "", rda$value)
rda$value<- gsub("삭제된 메시지입니다", "", rda$value)
rda$value<- gsub("사진", "", rda$value)
rda$value<- gsub("저도", "", rda$value)
rda$value<- gsub("저는", "", rda$value)
rda$value<- gsub("앗 ", "", rda$value)
rda$value<- gsub("하셨나요", "", rda$value)
rda$value<- gsub("음 ", "", rda$value)
rda$value<- gsub("오 ", "", rda$value)
rda$value<- gsub("혹시", "", rda$value)
rda$value<- gsub("헐 ", "", rda$value)
rda$value<- gsub("해서", "", rda$value)

np$value<- gsub("들어왔습니다", "", np$value)
np$value<- gsub("나갔습니다", "", np$value)
np$value<- gsub("넵", "", np$value)
np$value<- gsub("안녕하세요", "", np$value)
np$value<- gsub("네", "", np$value)
np$value<- gsub("교수님", "", np$value)
np$value<- gsub("아 ", "", np$value)
np$value<- gsub("하 ", "", np$value)
np$value<- gsub("감사합니다", "", np$value)
np$value<- gsub("삭제된 메시지입니다", "", np$value)
np$value<- gsub("사진", "", np$value)
np$value<- gsub("저도", "", np$value)
np$value<- gsub("저는", "", np$value)
np$value<- gsub("앗 ", "", np$value)
np$value<- gsub("하셨나요", "", np$value)
np$value<- gsub("음 ", "", np$value)
np$value<- gsub("오 ", "", np$value)
np$value<- gsub("혹시 ", "", np$value)
np$value<- gsub("헐", "", np$value)
np$value<- gsub("해서", "", np$value)
np$value<- gsub("저희", "", np$value)

#데이터 합치기
class_talk_ <- bind_rows(rda, np) %>% relocate(
  class, .before = value
)

#명사 기준 토큰화
class_talk <- class_talk %>% unnest_tokens(input=value, output = word,
                                       token = extractNoun)

#빈도수 확인
class_talk_frequency <-class_talk %>% count(class, word) %>% 
  filter(str_count(word)>1)

#TOP10 cut
top10 <- class_talk_frequency%>%group_by(class)%>%slice_max(n, n=10)%>% print(n=Inf) 

#빈도수 상위 10개에 대한 막대 그래프 만들기
theme_set(theme_grey(base_family='NanumGothic'))

ggplot(top10, aes(reorder_within(word,n,class),n,fill=class))+
  geom_bar(stat="identity")+coord_flip()+
  facet_wrap(~class,scales="free_y")+
  scale_x_reordered()+
  labs(x=NULL)

# 와이드 프레임으로 변경
class_talk_frequency_wide <- class_talk_frequency %>%
  pivot_wider(names_from = class,
              values_from=n,
              values_fill = list(n=0))

#단어 비중 구하기
class_talk_frequency_wide <- class_talk_frequency_wide %>%
  mutate(ratio_np=((np+1)/(sum(np+1))),
         ratio_rda=((rda+1)/(sum(rda+1))))

#오즈비 구하기
class_talk_frequency_wide <- class_talk_frequency_wide %>%
  mutate(odds_ratio=ratio_rda/ratio_np)

#오즈비가 가장 높은 10개 단어와 가장 낮은 10개 단어 추출하여 top10 만들기
top10 <- class_talk_frequency_wide %>% filter(rank(odds_ratio)<=11|rank(-odds_ratio)<=10)%>%
  arrange(-odds_ratio)

#막대 그래프 그리기
top10 <- top10 %>% mutate(class=ifelse(odds_ratio>1,"알데분","신상품"),
                          n=ifelse(odds_ratio>1,rda,np))

#상대적으로 많이 사용된 단어 빈도 막대그래프
ggplot(top10, aes(x=reorder_within(word, n, class),n,fill=class))+
  geom_bar(stat="identity")+coord_flip()+
  facet_wrap(~class, scales="free_y")+
  scale_x_reordered()+labs(x=NULL)

#문맥 찾아보기
#문장 단위로 토큰화하기
talk_sentence <- class_talk_ %>% as_tibble() %>%
  unnest_tokens(input = value, output = sentence, token="sentences")

talk_sentence %>% filter(class=="rda"&str_detect(sentence,"데이터"))
talk_sentence %>% filter(class=="np"&str_detect(sentence,"미팅"))

#중요도가 비슷하고 빈도가 높은 단어 살펴보기
#각 연설문에서 빈도수가 5회 이상이면서 중요도가 비슷한 단어 추출
#두 연설문에서 모두 강조한 단어 확인
class_talk_frequency_wide %>% filter(rda>=5&np>=5)%>%
  arrange(abs(1-odds_ratio))%>%head(10)


