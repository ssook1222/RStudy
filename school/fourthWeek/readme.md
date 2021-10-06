## 4주차 실시간 강의 실습 정리 

### 문제 1. ggplot2에서 midwest 데이터를 불러와서 같은 이름의 데이터 프레임을 만드시오.
```r
midwest <- ggplot2::midwest 
```

### 문제 2. popadults는 해당 지역 성인인구, poptotal은 해당 지역 전체 인구를 의미한다. 지역별 '미성년 인구비율' 변수(percyouth)를 만드시오.
```r
midwest<-midwest%>%mutate(percyouth=((poptotal-popadults)/poptotal)*100)
```
### 문제 3. percyouth가 제일 높은 다섯 개 지역을 구하시오.
```r
midwest%>%select(county,percyouth)%>%arrange(-percyouth)%>%head(5)

(midwest%>%arrange(-percyouth)%>%head(5))$county
```
특정 변수의 측정값을 뽑으니 **select**를 사용
select 사용이 중요!

### 문제 4. 새로운 변수(group)을 만들고 유형별 빈도를 구하시오.
```r
midwest<-midwest%>%mutate(group=ifelse(percyouth>=40,"large",ifelse
                                       (percyouth>=30&percyouth<40,"middle","small")))
table(midwest$group)
```
table로 빈도수를 본 결과, 한 쪽으로 쏠렸으므로 기준값을 조정할 필요가 있음.

### 문제 5. midwest_add.csv를 같은 이름의 데이터 프레임으로 저장한 후, 
### midwest와 midwest_add를 county 변수와 region 변수를 기준으로 통합하시오.

```r
midwest_add<-read.csv("./school/data/midwest_add.csv",fileEncoding ="euc-kr",stringsAsFactors = F)
table(is.na(midwest_add$senior)) #결측치 확인
midwest<-left_join(midwest,midwest_add,by=c("county"="region"))
```
county에 중복되는 사례가 있어서 join시 중복 사례에 같은 값이 붙다보니 case 수가 급증함.    
county와 region은 같은 변수이지만, 문제는 **이 두 변수에 대한 측정값이 중복**으로 나옴.
중복된 값끼리 조합을 이루어서 원래는 4개의 케이스만 만들어져야 하는데 중복으로 나오기에     
총 16개의 케이스가 만들어짐. (4*4=16)

이 문제를 distinct로 해결하려 하였으나...

### 문제 6. 통합 midwest 데이터 프레임에 중복 사례가 있다면 제거하라.
```r
midwest<-midwest%>%distinct(PID,county,state,.keep_all = T)
```
변수 결과 동일하게 나오나, **중복 사례가 있는 경우 중복된 케이스의 첫 행 1개**만 나옴.    
따라서 unique한 식별값이 아니면 함부로 join을 하면 안 됨.     
pid를 기준으로 join을 진행해야 함.   

### 문제 7. 주 별로 senior 인구수 합계를 구하시오.
방법1
```r
midwest%>%group_by(state)%>%summarise(sum_senior=sum(senior,na.rm = T))
```

방법2
```r
midwest%>%filter(!is.na(senior))%>%group_by(state)%>%summarise(sum_senior=sum(senior))
```

**groupby**에서는 **summarise 까먹지 말구 사용**하자!     
**is.na** 기억해두자!

### 문제 8. tidyr 패키지 설치 후, 결측치가 있는 사례 제거하기
```r
library(tidyr)
midwest<-midwest%>%drop_na()
```
dropna는 na(결측치)를 제거하는 함수이다.

### 문제 9. 평균 아시아계 인구수가 가장 적은 category 세 개를 구하시오.
```r
midwest%>%select(category,popasian)%>%arrange(mean(popasian))%>%head(3)
```
