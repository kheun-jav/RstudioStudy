#연습문제

#(1)
mpg_ch<-tibble(mpg) |> 
  select(where(is.character))
mpg_ch |> 
  print(n = 4)
#(2)
m_f<-mpg_ch |> 
  mutate(across(-c(1,2), as.factor))
m_f |> 
  print(n = 3)
#(3)
m_f |> count(trans)

#(4)
#str_sub로 묶으려는 변수들의 공통지점을 찾아 변형
mpg_am<-mpg |> 
  mutate(am = case_when(
    str_sub(trans, 1, 1) == "m" ~ "Manual",
    str_sub(trans, 6, 6) == "l" ~ "Auto-T1",
    .default = "Auto_T2"
  ))
mpg_am |> count(am)

#(5)
#cyl을 요인으로 바꿔야 fill이 적용됨
mpg_am |> 
  filter(cyl != 5) |> 
  ggplot(aes(x = am, fill = factor(cyl))) +
  geom_bar()+
  labs(x = "Type of transmission", y = NULL,
       fill = "cyl")
#Auto-T1 차량은 8 , 6 , 4기통 순위로 많다
#Auto-T2에 기통수는 큰 영향이 없는 걸로 보인다(거의 균등한)
#Manual은 4, 6, 8 순위로 많다
#영향력이 없을 것으로 보이는 T2를 제외하면 오토차량은 많은 기통을 사용하는 경향이 있고,
#수동 차량은 적은 기통을 사용하는 경향이 있다.
#따라서 차량 기종과 엔진 기통 사이에는 유의미한 관계가 있을 것으로 보인다
mpg_am |> 
  filter(cyl != 5) |> 
  ggplot(aes(x = am, fill = factor(cyl))) +
  geom_bar(position = "dodge")+
  labs(x = "Type of transmission", y = NULL,
       fill = "cyl")
#dodge 포지션의 막대그래프를 추가로 살펴보면
#T2는 8기통을 가장 적게 사용하는 것으로 나타났다.
#하지만 trand를 바꿀 정도의 엄청난 차이를 보이는 것이 아니므로
#영향력 있는 범주가 아닐 것으로 예상된다
#따라서 범주를 제외하거나 Auto_T1에 흡수하여 비교하는 것이 좋아 보인다
#분석 결과는 위와 동일하다

mpg_am |> 
  filter(cyl != 5) |> 
  ggplot(aes(x = factor(cyl), y=after_stat(prop), group = 1))+
  geom_bar(fill = "steelblue")+
  facet_wrap(vars(am))+
  labs(x = "Number of Cylinders", y = "Proportion")
#Auto_T1은 8, 6, 4 순으로 높은 비율을 가지고 있다
#Auto_T2는 8기통이 조금 적고, 4 6기통이 비슷한 비율을 가지고 있다
#Manual은 4, 6, 8 순으로 높은 비율을 가지고 있다

#(6)
mpg_am |> 
  filter(cyl != 5) |> 
  ggplot(aes(x = hwy, y = factor(cyl)))+
  geom_boxplot()+
  labs(x = "Highway MPG", y = "Number of Cylinders")
#(7)
mpg_am |> 
  mutate(drv = if_else(drv == 4, "4WD", "F or R")) |> 
  ggplot(aes(x = factor(cyl), y = hwy, fill = drv))+
  geom_boxplot()+
  labs(x = "Number of Cylinders", y= "Highway MPG")

mpg_am |> 
  mutate(drv = if_else(drv == 4, "4WD", "F or R")) |> 
  ggplot(aes(x = factor(cyl), y = hwy))+
  geom_boxplot()+
  facet_wrap(vars(drv))+
  labs(x = "Number of Cylinders", y= "Highway MPG")
