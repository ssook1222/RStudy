
# Weather


## 📍 학교 실습 내용 정리

2020년 서울 종로구 송현동 기상관측소에서 측정한 다양한 일별 기상데이터 파일인 **weather.csv**를 사용해 간단한 데이터 전처리를 하는 실습


## 📍 실습 문제별 사용 코드 보기

### 문제 1. weather.csv를 불러와서 weather라는 데이터 프레임을 만드시오.
```r
weather<-read.csv("./school/data/weather.csv",fileEncoding ="euc-kr",stringsAsFactors = F)
```
위 코드 중 weather.csv가 저장된 위치는 다를 수 있다.     
또한, OS에 따라 인코딩 방식이 다를 수 있다.

### 문제 2. 측정 척도가 문자인 변수는 무엇인가?
```r
str(weather)
```
실행 결과를 보면 알겠지만, **일시**와 **요일.구분**이 문자형으로 출력된다.

### 문제 3. '일시' 변수에 대해 측정 척도를 문자에서 날짜로, '요일.구분'에 대해 범주로 바꾸시오.

```r
weather$일시<-as.Date(weather$일시)
weather$요일.구분<-as.factor(weather$요일.구분)
str(weather)
```
**as.Date()** 를 이용해 날짜로 변수 형태 변경, **as.factor()** 를 이용해 범주로 형태로 변경

### 문제 4. 일시 변수를 이용해서 요일(월~일)을 파악한 후 이를 요일이라는 이름의 변수에 지정하라.   그리고 요일 변수의 척도를 문자에서 범주로 바꾸어라.

```r
weather$요일<-weekdays(weather$일시)
weather$요일<-as.factor(weather$요일)
```

범주로 변경하면 **총 7개의 범주**로 나뉘어진다.


### 문제 5. 14개 변수에 대해 summary()를 통해 검토해 보시오.
```r
summary(weather)
```
이 출력결과에서 **NA** 's로 표시된 것은 **결측치**들의 개수

### 문제 6. '일강수량' 변수에 대해 분산을 구해 보시오.
분산 등 기술 통계량을 구하기 위해서는 결측치를 사전에 제거해야 한다.     
아래와 같이 **na.rm=T**를 사용해 제거하자.
```r
var(weather$일강수량,na.rm=T) //na를 제거하겠다는 의미.
```

### 문제 7. 요일과 요일.구분에 대해 빈도수를 각각 구하시오

- 빈도수 확인

```r
table(weather$요일.구분)
weather$요일<-factor(weather$요일,levels=c("월요일","화요일","수요일","목요일","금요일","토요일","일요일"))
table(weather$요일)
```
c() : 컴바인 함수, 해당 함수를 이용해 levels를 지정해준다.     
지정해준 레벨로 요일별로 재정렬이 가능하다.

> **c() 함수의 추가적인 사용법**
```r
v1<-c(1,3,4,5,6)
v2<-c(1:5) //연속된 수 입력
```

### 문제 8. 요일과 요일.구분을 동시에 고려한 qplot을 그려보시오.
```r
library(ggplot2)
qplot(data=weather,요일,fill=요일.구분)+theme(text=element_text(size = 12,family = "NanumGothic"))
```

### 문제 9. 평균기온과 평균.상대습도에 대해 히스토그램을 그리시오.
```r
hist(weather$평균기온, breaks = seq(-20,50,by=1))
hist(weather$평균.상대습도, breaks = seq(0,100,by=1))
```

