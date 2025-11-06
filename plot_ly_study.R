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
economics |> print(n=2)

economics |> 
  plot_ly(x = ~date, y = ~unemploy) |> 
  add_lines()
#date의 순서로 입력되어 있음

#2008 ~ 20213까지의 umemploy의 월별 시계열 그래프를
#연도별로 구분해서 작성
economics |> 
  mutate(year = year(date), month = month(date)) |> 
  filter(between(year, 2008, 2013)) |> 
  plot_ly(x = ~month, y = ~unemploy) |> 
  add_lines(color = ~as.factor(year),
            linetype= ~as.factor(year))
#lubridate 패키지 필요(tidyverse의 core 패키지로 변경됨)
#date 자료형의 자료를 year(), month()를 사용하여 년 / 월로 분리 가능
#between()을 사용하여 2008 ~ 2013년도 자료만 filtering
#plot_ly로 선그래프 작성 : color 옵션에 년도 넣을때 요인으로 변환

#분석 : 2008년 실업자수 급증, 2009년도 증가하다 2010년 이후로는 감소하는 추세를 보인다
#2008년 금융위기 사태가 원인으로 보임.

#mtcars의 변수 disp와 mpg의 선그래프 작성
p6 <- plot_ly(mtcars, x = ~disp, y = ~mpg)
#add_lines() 사용
p6 |> 
  add_lines()
#add_path() 사용
p6 |> 
  add_paths()
#add_path()를 사용하여 
#add_lines()와 같은 형태의 그래프 작성
mtcars |> 
  arrange(desc(disp))|> 
  plot_ly(x = ~disp, y = ~mpg) |> 
  add_paths()

#add_segments() 사용
df<- tibble(x1 = c(1,1), x2 = c(4,4),
                 y1 = c(2,3), y2 = c(3,4))
df
df |> 
  plot_ly() |> 
  add_segments(x = ~x1, xend = ~x2,
               y = ~y1, yend = ~y2)
#slope_graph 작성
install.packages("CGPfunctions")
data(newgdp, package = "CGPfunctions")
newgdp |> head()

#1970년과 1979년의 GDP 수치를 나라로 탐색하기 위해 fivot_wider() 사용
gdp_wider<-newgdp |> 
  pivot_wider(names_from= Year, values_from = GDP)
gdp_wider |> head()

#add_segments()로 그래프 작성
gdp1<-plot_ly(gdp_wider) |> 
  add_segments(x = 1, xend = 2,
               y = ~Year1970, yend = ~Year1979,
               color = I("gray"))
gdp1
#라벨 추가 -> add_annotations()
gdp2<-gdp1 |> 
  add_annotations(x = 1, y = ~Year1970,
                  text = ~paste(Country, Year1970),
                  showarrow = FALSE, xanchor = "right")
gdp2
gdp3<- gdp2 |> 
  add_annotations(x = 2, y = ~Year1979,
                  text = ~paste(Country, Year1979),
                  showarrow = FALSE, xanchor = "left")
gdp3
#x축 y축 라벨 및 눈금 조절
gdp3 |> 
  layout(
    xaxis = list(
      tickvals = c(1,2),
      ticktext = c("1970", "1979")
    ),
    yaxis = list(
      title = "",
      showgrid = FALSE,
      showticklabels = FALSE
    )
  )
#x축에서 다듬을 내용들
#vals(구간)을 1 ~ 2로 설정
#해당 구간의 text를 1970, 1979로 변경
#y축에서 다듬을 내용들
#title 제거
#grid(눈금선), ticklabels(x,y축 구간 숫자) 제거

#각 나라별 년도의 변화에 따른 gdp 증감폭을 쉽게 파악할 수 있음

#dumbbell chart 작성
gdp_wider |> 
  mutate(Country = fct_reorder(Country, Year1970)) |> 
  plot_ly() |> 
  add_segments(x = ~Year1970, xend = ~Year1979,
               y = ~Country, yend = ~Country,
               color = I("gray"),
               showlegend = FALSE) |> 
  add_markers(x = ~Year1970, y = ~Country,
              color = I("blue"), name = "1970 GDP") |> 
  add_markers(x = ~Year1979, y = ~Country,
              color = I("red"), name = "1979 GDP") |> 
  layout(
    xaxis = list(
      title ="GDP"
    )
  )
#trace legend 제거 -> showlegend = FALSE를 add_...()함수안에 추가
#1970, 1979를 각각 add_markers()로 trace 생성  

#예제
#Mass 패키지의 df Card93
library(plotly)
library(tidyverse)

data(Cars93, package="MASS")
p1<-Cars93 |> 
  plot_ly(x = ~Weight, y = ~MPG.city,
          text = ~Make,
          hoverinfo = "text") |> 
  add_markers()
#MPG.city 값이 35가 넘는 세자료의 회귀직선과
#모든 데이터를 추가하는 회귀직선을 추가하라
library(broom)
fit1<- lm(MPG.city ~ Weight,Cars93)
fit2<- update(fit1, data = Cars93 |> filter(MPG.city <35))
#update()를 사용하여 fit1의 자료 중 outliers를 제외하여 재적합
df_fit1<- augment(fit1)
df_fit2<- augment(fit2)
#augment()를 이용하여 회귀직선 추정
df_fit1 |> 
  plot_ly(x = ~ Weight, y = ~MPG.city) |> 
  add_markers(text = ~Cars93$Make,
              hoverinfo = "text",
              showlegend = FALSE) |> 
  add_lines(y = ~.fitted, color = I("red"),
            name = "All data") |> 
  add_lines(data = df_fit2, y = ~.fitted, 
            color = I("blue"),
            name = "Exclude Outliers")
#ggplot2와 다르게 직접 회귀직선을 적합하고 해당 적합 결과(.fitted)를 add_lines()에 적용시켜야함

#(2) 두 데이터를 합쳐서 한번에 출력
df_fit1 |> 
  bind_rows(df_fit2, .id = "group") |> 
  plot_ly(x = ~Weight, y = ~MPG.city) |> 
  add_markers(showlegend = FALSE,
              colors = c("red", "blue")) |> 
  add_lines(y = ~.fitted, color = ~group)

#히스토그램 작성
diamonds |> 
  plot_ly(x = ~cut) |> 
  add_histogram()

#수평방향 그래프 작성
diamonds |> 
  plot_ly(y = ~cut) |> 
  add_histogram(orientation = "h")

#add_bars() 사용
diamonds |> 
  count(cut) |> 
  plot_ly(y = ~cut, x = ~n) |> 
  add_bars(orientation = "h")

#price의 히스토그램 작성
diamonds |> 
  plot_ly(x = ~price) |> 
  add_histogram()

#구간 사용자 지정
diamonds |> 
  plot_ly(x = ~price) |> 
  add_histogram(nbinsx = 50)

#add_bars()로 연속형 변수의 히스토그램 작성
diamonds |> 
  mutate(price_gp = cut_width(price, width = 200)) |>
  count(price_gp) |> 
  plot_ly(x = ~price_gp, y = ~n) |> 
  add_bars(width = 2) |> 
  layout(xaxis = list(showticklabels = FALSE))
#layout()함수로 구간 라벨을 지우지 않으면 
#price_gp의 모든 구간명이 지저분하게 나타남

#mpg 데이터
mpg_am<- mpg |> 
  mutate(am = if_else(str_sub(trans, 1, 1) == "a", "auto", "manual"))
#그룹 막대 그래프(디폴트)
mpg_am |> 
  plot_ly(x= ~am, color = ~drv) |> 
  add_histogram()

#쌓은 형태의 막대그래프
mpg_am |> 
  plot_ly(x = ~am, color = ~drv) |> 
  add_histogram() |> 
  layout(barmode ="stack")
#그룹간 비교가 어려운 형태

#상대도수 그래프 작성
mpg_am |> 
  count(am, drv) |> 
  group_by(am) |> 
  mutate(prop = n/sum(n)) |> 
  plot_ly(x = ~am, y = ~prop,
          color = ~drv) |> 
  add_bars() |> 
  layout(barmode = "stack")

#예제
#(1)
mpg |> 
  count(trans)
#(2)
mpg_trans<-mpg |>
  mutate(type = 
  case_when(
    str_sub(trans,1,1) == "m" ~ "Manual",
    str_sub(trans,6,6) == "l" ~ "Auto_T1",
    TRUE ~ "Auto_T2"
  ))

#add_histogram()으로 작성
h1<-mpg_trans |> 
  plot_ly(x = ~factor(type)) |> 
  add_histogram()
h1
#add_bars()로 작성
h2<-mpg_trans |> 
  count(type) |> 
  plot_ly(x = ~factor(type), y = ~n) |> 
  add_bars() |> 
  add_text(text = ~n,
           textposition = "top") |> 
  hide_legend()
h2
subplot(h1, h2, shareY = TRUE)

#(3)
#절대 도수에 의한 막대 그래프
mpg_trans |> 
  filter(cyl != 5) |> 
  plot_ly(x = ~factor(type), color = ~ factor(cyl)) |> 
  add_histogram()

#상대 도수에 의한 막대 그래프
mpg_trans |> 
  filter(cyl != 5) |> 
  count(type, cyl) |> 
  group_by(cyl) |> 
  mutate(prop = n/sum(n)) |> 
  plot_ly(x = ~factor(cyl), y = ~prop,
          color = ~factor(type)) |> 
  add_bars() |> 
  layout(barmode ="stack")

p3<-mpg_trans |> 
  filter(cyl != 5) |> 
  ggplot()+
  geom_bar(aes(x = factor(cyl), y = after_stat(prop),
               group = 1), fill = "steelblue")+
  facet_wrap(vars(factor(type)))+
  labs(x = "cyl", title = "ggplotly로 작성한 그래프")
ggplotly(p3)
#ggplotly() 함수로 ggplot 그래프 plotly 형식으로 변환

#Box trace
#수직
p1<- plot_ly(mpg, y = ~hwy, color = I("black"))
p1 |> add_boxplot(x = "")
#수평
p2<- plot_ly(mpg, x = ~hwy, color = I("black"))
p2 |> add_boxplot(y = "")

#cyl 그룹별 hwy의 boxplot 작성
p1 |> 
  filter(cyl %in% c(4,6,8)) |> 
  add_boxplot(x = ~cyl)
#기초 plot_ly()를 작성하고 filtering 할 수 있다

#class 그룹별 boxplot
p1 |> 
  add_boxplot(x = ~class)
#hwy의 중앙값 크기에 따른 class 범주 순서 재배열
p1 |> 
  add_boxplot(x = ~reorder(class, hwy)) |> 
  layout(xaxis = list(title = "class"))

#예제
data(Cars93, package = "MASS")
#(1)
Cars93 |> 
  plot_ly(x = ~MPG.city, y = "") |> 
  add_boxplot() |> 
  add_markers(x = mean(Cars93$MPG.city),
              size = 30,
              color = I("red")) |> 
  hide_legend()
#(2)
c1<-Cars93 |> 
  filter(Cylinders %in% c(4,6,8)) |> 
  mutate(Cylinders = factor(Cylinders, levels = c(4,6,8))) |> 
  plot_ly(x = ~Price, y = ~ Cylinders) |> 
  add_boxplot()
c1
c2<-Cars93 |> 
  filter(Cylinders %in% c(4,6,8)) |> 
  mutate(Cylinders = factor(Cylinders, levels = c(4,6,8))) |> 
  plot_ly(x = ~MPG.city, y = ~ Cylinders) |> 
  add_boxplot()
c2
subplot(c1,c2, nrows = 2, titleX = TRUE, titleY = TRUE,
        margin = 0.08) |> 
  hide_legend()
