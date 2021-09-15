## 2주차 실시간 실습 정리

### 1. 평균기온이 27도 이상인 날이 몇 일인가?
```r
table(weather$평균기온>=27)
```
### 2. 평균기온이 10도 이상이고 20도 이하인 날이 몇 일인가?
```r
table(weather$평균기온>=10&weather$평균기온<=20)
```

### 3. 일강수량이 0인 날은 얼마나 되는가?
```r
table(weather$일강수량==0)
```

### 4. 일강수량 변수의 측정 값이 NA(결측치)인 날은 얼마나 되는가?
```r
summary(weather$일강수량)
table(is.na(weather$일강수량))
```

cf) 일강수량이 결측치가 아닌 것 table(!is.na(weather$일강수량))

### 5. 월요일, 화요일, 수요일은 몇 일인가?
```r
Sys.setlocale("LC_TIME", "ko_KR.UTF-8")
weather$일시<-as.Date(weather$일시)
weather$요일<-weekdays(weather$일시)

weather$요일<-as.factor(weather$요일)
weather$요일<-factor(weather$요일,levels=c("월요일","화요일","수요일","목요일","금요일","토요일","일요일"))

table(weather$요일=="월요일"|weather$요일=="화요일"|weather$요일=="수요일")
table(weather$요일 %in% c("월요일","화요일","수요일"))
```
### 6. 최고기온이 30도 보다 높고, 평균.상대습도는 80보다 낮은 날은 몇 일인가?
```r
table(weather$최고기온>30&weather$평균.상대습도>80)
```
### 7. 최저기온이 -10보다 낮거나, 합계.일조시간이 1시간 미만인 날은 몇 일인가?
```r
table(weather$최저기온< -10|weather$합계.일조시간<1.0)
```

**-와 <를 붙여 쓰는 순간 할당**이 되어 버리므로 주의하자!

### weather의 복제 본인 weather_new 데이터 프레임을 만들어라.
```r
weather_new<-weather
```
### 8. 요일.구분은 요일구분으로, 평균.현지기압은 평균기압으로 변수명을 바꾸시오.
```r
library(dplyr)
weather_new<-rename(weather,요일구분=요일.구분, 평균기압=평균.현지기압)
```
### 9. 새로운 변수 요일구분과 관련해서 출력 순서를 기존의 평일-휴일에서, 휴일-평이로 변경하시오.
```r
table(weather$요일.구분) #before

weather_new$요일구분<-factor(weather_new$요일구분,levels=c("휴일","평일"))
table(weather_new$요일구분) #after
```
### 10. 일강수량이 0으로 측정된 경우 이 값을 NA로 바꾸시오.
```r
weather_new$일강수량<-ifelse(weather_new$일강수량==0,NA,weather_new$일강수량)

```
### 11. 평균기압에 대한 결측치는 몇 개인가?
```r
table(is.na(weather_new$평균기압))
```
### 12. 결측치를 제외한 평균기압 평균은 얼마인가(유효숫자 소수 둘째자리)?
```r
round(mean(weather_new$평균기압,na.rm=T),digits=2)
```
### 13. 평균기압이 NA인 경우 이를 문제 12에서 구한 평균값으로 대체하시오.
```r
weather_new$평균기압<-ifelse(is.na(weather_new$평균기압)==TRUE,
                         round(mean(weather_new$평균기압,na.rm=T),digits=2)
                         ,weather_new$평균기압)

```
