#13장 2번
library(tidyverse)
data(Cars93, package = "MASS")
#데이터 호출
str(Cars93)
#1993년에 출시된 자동차들의 스펙 정보 및 가격에 대한 데이터

#7)
#상자그림 작성
mc<-Cars93 |> 
  ggplot(aes( x= MPG.city))+
  geom_boxplot()
mc
#확인시 3개의 outliers가 관찰됨
#ggplot_build()를 사용하여 해당 이상치 추출
mc_out <- ggplot_build(mc)[[1]][[1]]$outliers[[1]]
mc_out
#산점도 작성 및 라벨 추가
p1<-ggplot(Cars93, aes(x = Weight, y = MPG.city))+
  geom_jitter()
p1
p2<-p1 + geom_text(data = filter(Cars93, MPG.city %in% mc_out),
               aes(label = paste(Manufacturer, Model)),
               vjust = "top", hjust = "left")
p2
#Weight가 높을수록 MPG.city가 낮아지는 추세를 가진 그래프에서
#낮은 무게임에도 불구하고 높은 MPG.city를 가진 세 이상치는
#Geo Metro, Honda Civic, Suzuki Swift 세 기종이다

#8)
p2 + 
  geom_smooth(aes(color = "Use all data"),
    method = "lm", se = FALSE)+
  geom_smooth(data = Cars93 |> 
                filter(! MPG.highway %in% mc_out),
              aes(color = "Exclude Outliers"),
              method = "lm", se = FALSE)+
  labs(color = NULL)
#그래프 내부에서 data = 를 사용하여 범위를 재설정 할 수 있으며,
#파이프도 사용 가능하다

#9)
Cars93 |> 
  select(Cylinders) |>
  group_by(Cylinders) |> 
  count()

b1<- Cars93 |> 
  filter(Cylinders %in% c(4, 6, 8)) |> 
  ggplot(aes(x = Cylinders, y = Price))+
  geom_boxplot()
b1
cyl_out<- ggplot_build(b1)[[1]][[1]]$outliers
cyl_out

b1 + geom_text(data = (Cars93 |>  
                       filter(Cylinders == 4 & Price == max(cyl_out[[1]]))),
               aes(label = Make), vjust = "bottom") +
  geom_text(data = Cars93 |> 
              filter(Cylinders == 6 & Price == max(cyl_out[[2]])),
            aes(label = Make), vjust = "bottom")
