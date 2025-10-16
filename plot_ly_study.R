#plotly
install.packages("plotly")
library(plotly)
library(tidyverse)
mpg |> 
  plot_ly(x = ~displ, y = ~hwy)

#대화형 산점도 그래프 : hoverinfo의 활용용
mpg |> 
  plot_ly(x = ~displ, y = ~hwy) |> 
  add_markers(hoverinfo = "text",
              text = paste(mpg$manufacturer, mpg$model))

#산점도의 문제점 : 같은 값을 가지는 자료가 많은 경우

#sol1) 점의 투명도를 높여서 해결한다
#plotly에서는 subplot을 사용하여 patchwork의 기능을 사용한다
p1 <- plot_ly(mpg, x = ~cty, y = ~hwy)
p11 <- p1 |> 
  add_markers(name = "default")
p12 <- p1 |> 
  add_markers(alpha = 0.2, name = "alpha")
subplot(p11,p12)
#margin 및 배치 방식 조정:subplot 옵션 사용
subplot(p11, p12, margin = 0.05)
subplot(p11, p12, nrows = 2, margin = 0.05, shareX = TRUE)

#sol2) 자료 흩뿌리기(jittering)
#ggplot과 달리 plotly는 변수 대상으로 jittering 해야함
p2<-mpg |> 
  mutate(cty_n = jitter(cty, amount = 0.5),
         hwy_n = jitter(hwy, amount = 0.5)) |> 
  plot_ly(x = ~cty_n, y = ~hwy_n) |> 
  add_markers(name = "jitter")
subplot(p11, p2, margin = 0.05)

#color 매핑
p3<- plot_ly(mpg, x = ~displ, y = ~hwy)
#연속형(숫자형) 변수 cyl 매핑
p3 |> 
  add_markers(color = ~cyl)
#연속형 변수는 반응형으로 제작되지 않는다
#이산형 변수
p3 |> 
  add_markers(color = ~ as.factor(cyl))

#사용자 수동 지정
p3 |> 
  add_markers(color = I("red"))

#논리형 변수의 매핑
#2개의 trace(T/F) 생성
plot_ly(mtcars, x = ~disp, y = ~mpg) |> 
  add_markers(color = ~wt >=mean(wt))
#trace 설정시 조건문을 집어넣으면 T/F 2개의 trace가 생성됨됨
#주의) false가 먼저, true가 나중에
#만약 mutate를 사용하여 논리형 변수를 직접 만들었다면,
#true, false의 순서가 바뀐다
plot_ly(mtcars, x = ~disp, y = ~mpg) |> 
  add_markers(color = ~wt >= mean(wt),
              colors = c("red", "blue"))
mtcars |> 
  mutate(wt_label = if_else(wt >= mean(wt), 
                            "Heavy", "Light")) |> 
  plot_ly(x = ~disp, y = ~mpg) |> 
  add_markers(color = ~wt_label,
              colors = c("blue", "red"))

#hover 라벨을 mtcars의 행이름으로 변경
mtcars |> 
  mutate(wt_label = if_else(wt >= mean(wt),
                            "Heavy", "Light")) |> 
  plot_ly(x = ~disp, y = ~mpg) |> 
  add_markers(color = ~wt_label,
              colors = c("blue", "red"),
              hoverinfo = "text",
              text = rownames(mtcars))
#wt를 논리형 변수로 mutate후 
#plotly로 disp와 mpg간의 반응형 그래프 생성
#color에 wt가 평균보다 큰 값들(TRUE)를 blue로,
#작은값(FALSE)를 red로 설정
#text 형식으로 hover를 설정하고 해당 텍스트는 행이름(차종)으로 설정

#점의 모양 : symbol로 설정
p4<- plot_ly(mpg, x = ~displ, y = ~hwy)
#연속형 변수 cyl 매핑
p4 |> 
  add_markers(symbol = ~cyl)
#기존 ggplot에서는 오류가 발생하는 case(연속형 변수를 shape에 매핑)
#큰 효과를 보기는 어려워보임

#사용자 지정 : I()안에 모양 번호 지정
p41<- p4 |> 
  add_markers(name = "default")
p42<- p4 |> 
  add_markers(symbol = I(1), name= "I(1)")
subplot(p41, p42, margin = 0.05)

#산점도 점 크기 : size
#Animation 효과로 사용 가능
p5<-plot_ly(mpg, x = ~displ, y = ~hwy)
p51<- p5 |> 
  add_markers(size = ~cyl, name = "default")
p52<- p5 |> 
  add_markers(size = ~cyl, sizes = c(50,100),
              name = "custom")
subplot(p51,p52, margin = 0.05)

#사용자 지정 : I() 안에 필셀 단위로 크기 지정
p5 |> 
  add_markers(alpha = 0.3, size = I(20),
              color = I("black"))

#각 조합마다의 개수를 n 변수에 저장하고 중복제거
mpg_1<-mpg |> 
  group_by(displ,hwy) |> 
  mutate(n = n()) |> 
  distinct(displ, hwy, .keep_all = TRUE)
mpg_1 |> 
  select(manufacturer,model,displ,hwy,n)
#데이터 개수를 점의 크기에 적용하여 그래프 작성
#hover시 text에 해당 데이터의 개수를 표현
plot_ly(mpg_1,x = ~displ, y = ~hwy) |> 
  add_markers(size = ~n,
              hoverinfo = "text",
              text = paste(mpg_1$n, "data points"))

#선그래프
#add_lines()와 add_paths()활용
#economics = 1967.07 ~ 2015.04 까지의 미국 경제 지표 시계열 자료
data(economics, package="ggplot2")
str(economics)

economics |> 
  plot_ly(x = ~date, y = ~unemploy) |> 
  add_lines()
#date의 순서로 입력되어 있음

#2008 ~ 20213까지의 umemploy의 월별 시계열 그래프를
#연도별로 구분해서 작성
economics |> 
  mutate(year = year(date), month = month(date)) |> 
  filter(between(year, 2008, 2013)) |> 
  plot_ly(x = ~ month, y = ~unemploy) |> 
  add_lines(color = ~as.factor(year))
#date 자료형의 자료를 year(), month()를 사용하여 년 / 월로 분리 가능
#between()을 사용하여 2008 ~ 2013년도 자료만 filtering
#plot_ly로 선그래프 작성 : color 옵션에 년도 넣을때 요인으로 변환

#분석 : 2008년 실업자수 급증, 2009년도 증가하다 2010년 이후로는 감소하는 추세를 보인다
#2008년 금융위기 사태가 원인으로 보임.
