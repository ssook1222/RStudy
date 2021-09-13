
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
weather$요일<-as.factor(요일)
```

범주로 변경하면 **총 7개의 범주**로 나뉘어진다.

