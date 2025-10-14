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