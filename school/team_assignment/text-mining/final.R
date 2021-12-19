########## 신문비교 #########
aa.jungang <- readLines("./school/team_assignment/text-mining/data/jungang.txt", encoding = "euc-kr")
jungang <- aa.jungang %>% as_tibble() %>% mutate(newspaper = "jungang")

aa.hangyeore <- readLines("./school/team_assignment/text-mining/data/hangyeore.txt", encoding = "euc-kr")
hangyeore <- aa.hangyeore %>% as_tibble() %>% mutate(newspaper = "hangyeore")

## 신문 통합과 전처리 및 토큰화 ##
bind_newspaper <- bind_rows(jungang, hangyeore) %>% relocate(newspaper, .before = value)
library(stringr)
newspaper <- bind_newspaper %>% mutate(value = str_replace_all(value, "[^가-힣]", " "), value = str_squish(value))
library(tidytext)
library(KoNLP)
newspaper <- newspaper %>% unnest_tokens(input = value, output = word, token = extractNoun)

## 신문별 단어 빈도수 구하기 ##
n.frequency <- newspaper %>% count(newspaper, word) %>% filter(str_count(word) > 1)

## 빈도수 상위 10개 단어 데이터 만들기 ##
top10 <- n.frequency %>% group_by(newspaper) %>% slice_max(n, n = 10) %>% print(n = Inf)
top10 <- n.frequency %>% group_by(newspaper) %>% slice_max(n, n = 10, with_ties = F) %>% print(n = Inf)

## 막대 그래프 그리기 ##
library(ggplot2)
ggplot(top10, aes(reorder(word, n), n, fill = newspaper)) + geom_bar(stat = "identity") + coord_flip() + facet_wrap(~newspaper, scales = "free_y")

# 그래프별로 X축 항목 결과값을 다르게 하기 #
library(tidytext)
ggplot(top10, aes(reorder_within(word, n, newspaper), n, fill = newspaper)) + geom_bar(stat = "identity") + coord_flip() + facet_wrap(~newspaper, scales = "free_y") + scale_x_reordered() + labs(x = NULL)

n.frequency_wide <- n.frequency %>%
  pivot_wider(names_from = newspaper,
              values_from=n,
              values_fill = list(n=0))

n.frequency_wide <- n.frequency_wide %>%
  mutate(ratio_h=((hangyeore+1)/(sum(hangyeore+1))),
         ratio_j=((jungang+1)/(sum(jungang+1))))

n.frequency_wide <- n.frequency_wide %>%
  mutate(odds_ratio=ratio_h/ratio_j)

top_up_10 <- n.frequency_wide %>% filter(rank(-odds_ratio)<=13)%>%slice_max(odds_ratio, n=10,with_ties = F)
top_down_10<- n.frequency_wide %>% filter(rank(odds_ratio)<=13)%>%slice_max(odds_ratio, n=10,with_ties = F)
top10_<- bind_rows(top_up_10, top_down_10)

top10_<- top10 %>% mutate(class=ifelse(odds_ratio>1,"한겨례","중앙일보"),
                          n=ifelse(odds_ratio>1,hangyeore,jungang))

ggplot(top10_, aes(x=reorder_within(word, n, odds_ratio),
                   odds_ratio,fill=class))+
  geom_bar(stat="identity")+coord_flip()+
  facet_wrap(~class, scales="free")+
  scale_x_reordered()+labs(x=NULL)+
  geom_text(aes(label = odds_ratio))




