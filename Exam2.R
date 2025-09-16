library(tidyverse)
data(Cars93, package="MASS")
str(Cars93)
#Cars93의 EngineSize에 따른 히스토그램 작성
tibble(Cars93) |> 
  ggplot(aes(x = EngineSize, y= after_stat(density)))+
  geom_histogram(fill="red", bins = 15)+
  geom_density(color = "blue", linewidth = 1)+
  xlim(0, 7)+
  labs(x = NULL, title="Histogram of EngineSize")
#Cars93의 EngineSize에 따른 boxPlot 작성
tibble(Cars93) |> 
  ggplot(aes(x=EngineSize, y=""))+
  geom_boxplot()+
  labs(x = NULL, title="Boxplot of EngineSize")

#BoxPlot의 outlier에 설명변수 출력
bc<- Cars93 |> 
  ggplot(aes(x= EngineSize))+
  geom_boxplot()
bc

bc_out<-ggplot_build(bc)[[1]][[1]]$outliers[[1]]
Cars93 |> 
  filter(EngineSize %in% bc_out) |> 
  select(Manufacturer, Model)
#평균, 표준편차 구하기
Cars93 |> 
  filter(! EngineSize %in% bc_out) |> 
  summarise(mean = mean(EngineSize),
            sd = sd(EngineSize))
#EngineSize를 세 범주로 만든 CarSize 변수를 생성하고, 
#각각 Small, Mid, Large로 설정하여 bar그래프 그려보기
#case_when() -> switch문이랑 비슷
Cars <- Cars93 |> 
  mutate(CarSize = case_when(
    EngineSize <= 1.6 ~ "Small",
    EngineSize <= 2.0 ~ "Mid",
    EngineSize > 2.0 ~ "Large") |> 
      factor(levels = c("Small", "Mid", "Large"))
  )
Cars |> 
  ggplot(aes(x = CarSize))+geom_bar()

#MPG.city의 box_plot 작성
Cars |> 
  ggplot(aes(x=MPG.city, y = ""))+
  geom_boxplot()+
  stat_summary(fun = "mean", geom = "point",
               color="red", size = 3, stroke = 2)+
  ylab(NULL)

#점으로 표시된 세 관찰값의 변수로 이루어진 df 생성
#step1) boxplot 생성
bc2<- Cars |> 
  ggplot(aes(x=MPG.city))+
  geom_boxplot()
#step2) ggplot_build에서 outliers 인덱스
bc2_out <- ggplot_build(bc2)[[1]][[1]]$outliers[[1]]
bc2_out
#step3) filter에 적용시켜서 출력
tibble(Cars) |> 
  filter(MPG.city %in% bc2_out) |> 
  select(Manufacturer, Model, MPG.city, Weight) |> 
  arrange(desc(Weight))

#산점도 작성, outlier에 Model명 텍스트 달기
plot1<-Cars93 |> 
  ggplot(aes(x = Weight, y= MPG.city))+
  geom_jitter()