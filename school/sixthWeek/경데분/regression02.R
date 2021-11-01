#문제 11
str(lm_order)
#num이랑 int로 되어 있음 이는 계량척도
#visit~sat이 계량척도, 이 변수로 상관관계 확인

#상관관계 확인

#1. 시각화
install.packages("car")
library(car)

# 스캐터(산점도)
scatterplotMatrix(~visit+duration+morning+return+expense+age+income
                  +time+satisfaction, data=lm_order)
#plot을 보면 중앙 대각선은 각 측정값에 대한 분포임
#중앙 대각선 기준으로 딱 접으면 마주보고 있는 것은 같은 x,y 가 같은 그래프
#중앙을 보면 경향선이 있음
#(y1,x1)의 형태로 그래프가 그려짐 
#우상향은 양의 상관관계, 좌하향은 음의 상관관계

#상관관계를 파악할 때 시각적으로 한 번에 보여주긴 하지만, 주관이 너무 들어감. 
#엄격한 판단이 불가능. 그래서 통계적으로 p value 검정을 해야 됨.

#2. 통계적인 검정
library(psych)
corr.test(lm_order[,c(3:12)], #이 부분만 변경해주자. relocate로 변수끼리 모아놓고 그룹핑 하는 걸 추천
          method="pearson",
          alpha = 0.05,
          use="pairwise.complete.obs") #두 변수가 짝을 이룰 때 완전히 값이 있는 것들로만 이뤄라.
          #상관관계를 통계적으로 검정
#피어슨 상관관계, 유의계수 0.05
#corr.test(lm_order[,c(3:12)])
#행에 관계없이 3~12번째 열까지 데이터를 모두 선택하라.

#결과적으로 자기 자신과 자기 자신과의 상관관계는 다 1. (대각선)
#짝이 대각선을 기준으로 위, 아래가 갈리므로 한 쪽만 보면 됨.
#절댓값이 작을 수록 보통 상관관계 크기가 적음.

#p value가 유의계수보다 작으면 통계적으로 유의한 상관관계가 존재

#문제에서 상관관계로 유의한지 보여라라고 나오면
#pvalue와 상관계수를 이용해 답을 작성하면 됨.
#pvalue가 유의계수보다 작고, 상관계수의 절댓값이 크므로 (음/양)의 상관관계라고 작성하면 됨.

lm_order[4,5] #앞은 행의 번호를, 뒤는 열의 번호를. 
# 배열을 이용해 테이블의 특정 값을 불러 옴

#문제 12. 종속변수와 독립변수에 대한 인과관계를 수립해라.
#인과관계 가설 수립

# h1 : visit -> expense (+)
# h2 : duration -> expense (+)
# h3 : order -> expense (+)
# h4 : morning -> expense (+)
# h5 : return -> expense (-)
# h6 : age -> expense (+)

#p value를 보고 유의한지 확인한 후 상관계수를 확인
#가설은 상관관계보다 인간관계를 설명함.
#인과관계는 상관관계에 더해서 논리적인 설명 혹은 과학적인 설명이 가능해야 함.
#왜 가설이 맞는지 간단한 설명이 더해짐.


#문제 13
#회귀분석 실시 함수(아래는 다중회귀분석 예제)
lm_order.M1<-lm(expense~visit+duration+order+morning+return+age,data=lm_order)
#객체(여러 리스트가 모인 형태). 12개의 리스트

#문제 14 + 문제 15
#종속변수의 정규성을 검토하시오.
#1.시각화
plot(lm_order.M1)

#2.통계적 검토
ks.test(lm_order$expense, pnorm, mean =mean(lm_order$expense),
        sd=sd(lm_order$expense)) #kolmogrov smironov test : 정규성 검토
#p분포, expense(종속변수) 평균과 표준편차를 기반으로 정규성 검토 진행
#pvalue가 굉장히 적음. 
#도출된 pvalue는 사실상 2p-value이므로 2p-value를 유의계수와 비교
#**유의하지 않아야 정규성 조건 만족

#따라서 유의해서 정규성 조건을 만족하지 않음

#문제 15
#선형성과 등분산성
#plot을 그릴 때 첫번째, 세번째 그래프에서 빨간선이 수평으로 추정되어야 함.

#따라서 시각화된 도표만 봤을 때 선형성과 등분산성을 만족했다고 보기에는 어려워보임
#주관적으로 판단할 수밖에 없음

#플롯에 이상치로 나온 케이스의 인덱스가 보임.
#해당 인덱스(438,526,671)의 데이터를 제거하면 등분산성과 선형성이 개선될 것이라는 의미

#10,484,659에 해당되는 case는회귀식 추정에 지나치게 많은 영향을 미치므로 제거
#residuals VS Leverage 그래프의 10,484,659 (쿡의 거리)

#문제 16
#등분산성, 선형성에 영향을 미치는 케이스 제거
lm_order<-lm_order%>%filter(case!=517, case!=606, case!=760, #선형성
                            case!=15,case!=564,case!=748) #회귀분석에 너무 많은 영향

#그 후 13부터 반복하며 다시 점검하고 반복
#선형성, 등분산성, 정규성에 영향을 미치는 사례와 회귀 분석에 영향을 미치는 사례 제거 
lm_order<-lm_order%>%filter(case!=427, case!=464, case!=760,
                            case!=645,case!=867,case!=897)

lm_order<-lm_order%>%filter(case!=425, case!=257, case!=462,
                            case!=642,case!=862,case!=892)

lm_order<-lm_order%>%filter(case!=422, case!=459, case!=753,
                            case!=638,case!=856,case!=886)

# 이 과정을 계속 반복

#문제 17
#로그를 취해 정규성을 개선하고자 함.
#자연로그 - log() 로 변경
lm_order <- lm_order%>%mutate(ln_expense=log(expense))

#정규성 검토 진행
#유의하지 않아야 정규 분포를 만족
ks.test(lm_order$ln_expense, pnorm, mean =mean(lm_order$expense),
        sd=sd(lm_order$expense)) 

#여전히 2p-value가 a보다 유의함. 정규성 조건을 만족하지 못함

lm_order.M2<-lm(ln_expense~visit+duration+order+morning+return+age,data=lm_order)

#문제 18
#다중 회귀식 추정 결과 확인
summary(lm_order.M1)
#residual : 잔차
#intercept가 a 값(절편), 나머지 독립변수는 b1,b2..
# r에서는 2p value이므로 2a와 2p value를 비교
# ***=0.001, **=0.01, *=0.05, .=0.1, " "=1
# 아무리 p value가 유의해도 +,- 방향성이 다르면 채택 불가

summary(lm_order.M2)

#M1은 2,4,6 채택. M2는 1,2,4,6 채택
#M2의 모형 적합도는 더 떨어지나 가설 채택 수가 증가
#그래서 모형적합도(GoF)를 기준으로 모형을 채택할 지 등 기준을 설정해줘야 함.

#문제 19
#오차의 자기상관 검토

library(car)
#자기상관 테스트
durbinWatsonTest(lm_order.M1)
#p-value가 유의하지 않은게 오차의 자기상관이 없음.
#지금은 오차의 자기 상관이 약하게 있는 상태(autocorrelation 참고)
#독립성 조건 만족 불가.

#문제 20
#독립변수의 다중공선성을 검사하시오.
library(car)
vif(lm_order.M1)
#모두 5.3 기준값보다 작게 나옴. 다중공선성을 유발하는 변수는 없음.

#문제 21
#표준화 회귀계수 추정치를 가지고 추정
#H2,H4,H6만 채택되었으므로 유의한 회귀계수 추정치에 대해서만 표준화 회귀계수 추정치 절댓값 크기를 비교

install.packages("QuantPsyc")
library(QuantPsyc)

lm.beta(lm_order.M1)
#계수 추정치, 표준화 회귀계수 추정치
#근데 각 변수마다 측정단위가 다름. 그래서 어떤 변수가 상대적으로 더 중요한지
#표준화 회귀계수 추정치를 사용. 이를 이용해 더 상대적으로 중요한 변수를 알 수 있음.
#b2,b4,b6에 대해서만 비교
#morning이 상대적으로 중요. 

#문제22

#예측 시 추정된 회귀식을 이용해 예측을 진행

#가설검정과 예측은 좀 다름.
#추정된 회귀식을 작성시 pvalue 기준으로 유의한 변수만 작성.
#+,- 방향과 상관없음

#expense를 예측해보시오.
#인과관계 규명 외에도 예측도 하나의 목적
lm_order.predict<-read.csv("./school/data/predict.csv", fileEncoding ="euc-kr", stringsAsFactors = F)

# lm_order.M1에는 유의하지 않은 변수(return)가 있으므로 해당 변수의 값을 모두 0으로 변경
lm_order.predict$return<-0
#추정한 회귀식으로 예측
predict(lm_order.M1,newdata = lm_order.predict)


#문제 23
#더미변수 추가, 추가 후 정당성 확인

#정당성 확인법, 이 두 조건을 다 만족해야 함.
#1.추가 후 모델의 r^2 adj가 추가 전 모델의 r^2 adj보다 커야 함.
#2.r^2(r^2:추가후 모델 - r^2:추가 전 모델)가 F분포를 그려야 함. 그때 p value가 유의수준보다 작아야 함.

lm_order.M3 <- lm(expense~visit+duration+order+morning+return+age+gender+payment,data=lm_order)

summary(lm_order.M3) #증가하긴 했으나, 이것이 유의한지는
#f통계를 통해 확인해야 함.

#f통계로 확인
anova(lm_order.M1,lm_order.M3) #0.05보다 작으므로 유의한 증가력
#의미가 있는 추가였음. 종속변수를 설명해주는 역할을 한다는 의미

#문제 24
#M3가 채택된다면, 남성에 비해(준거집단, 0) 여성이, 신용카드나 간편결제가
#DV에 미치는 영향이 유의한지 확인하라(하나 남는 걸 준거집단으로)

#결제수단은 집단이 3개로 나뉘므로, dv는 2개(3-1)
#계좌이체에 비해 라고 적혀있으므로 계좌이체는 준거집단.
#간편결제는(1,0) / 신용카드는 (0,1) / 계좌이체는(0,0)이다.

#값을 변경할 때 factor(변수, level=c())를 이용해 순서를 잡아줘야 함.

#변경 전 구조 확인
str(lm_order$gender) 
str(lm_order$payment)

contrasts(lm_order$gender)
contrasts(lm_order$payment)
#앞에 위치한 변수를 준거집단으로 생각했다는 의미

#따라서 순서를 바꿔 범주형 척도를 변경해줘야 함.
lm_order$gender <- factor(lm_order$gender,levels=c("Male","Female"))
lm_order$payment <- factor(lm_order$payment,levels=c("계좌이체","간편결제","신용카드"))

#이렇게 한 후, M3를 다시 만들고 추정
lm_order.M3 <- lm(expense~visit+duration+order+morning+return+age+gender+payment,data=lm_order)
summary(lm_order.M3)

#더미변수는 준거집단과 비교했을 때 결과가 나옴.

#문제 25
#가입경로에 따라 모델을 구분하고자 한다.
#가입경로에 따라 구분해 두 개의 다중회귀식 subset을 만드시오.

#필터링해서 subset을 2개로 나눔
#lm 만들 때 subset 옵션에 조건을 넣어 만들어주면 됨.

#빈도수 체크
table(lm_order$path)

lm_order.M3_s1 <- lm(expense~visit+duration+order+morning+return+age+gender+payment,
                      data=lm_order,
                      subset=(path %in% c("Banner","Facebook")))

lm_order.M3_s2 <- lm(expense~visit+duration+order+morning+return+age+gender+payment,
                      data=lm_order,
                      subset=(path %in% c("Instagram","Kakao","Youtube")))

summary(lm_order.M3_s1) #1,2,4,6 유의
summary(lm_order.M3_s2) #1,2,4,6,7 유의
#전체가 아니라 부분으로 봤을 때 해석이 또 다름

#문제 26
# 새로운 변수 추가(timeXincome => 상호작용 변수)
# 그 후 설명력 비교

lm_order.M4 <- lm(expense~visit+duration+order+morning+return+age+gender+payment+time*income,data=lm_order)
#time*income = time, income, timeXincome
summary(lm_order.M4)

anova(lm_order.M3)
anova(lm_order.M4)
#income에 따라 다 증가하긴 하나
#time*income이 -임.
#집단을 2개로 쪼갰을 때 가입기간에 따라 다 증가하긴 하나
#증가의 폭이 다름. 가입기간이 길수록 소득 증가에 따라 expense 증가폭이 작음(둔화)





