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
lm_order.M1<-lm(expense~visit+duration+order+return+age,data=lm_order)
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










