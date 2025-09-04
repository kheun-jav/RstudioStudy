#ggplot2에 의한 자료 탐색
library(tidyverse)

#일변량 자료인 경우 (범주형 변수가 한 개인 경우)
#state.region data
str(state.region)
state.region[1:10]
#요인이 자료인 경우 - geom_bar()로 막대그래프 작성 
#geom_bar()는 디폴트가 count임
tibble(state.region) |> 
  print(n=3)
#수직 방향 그래프
tibble(state.region) |> 
  ggplot() + 
  geom_bar(aes(x= state.region)) +
  xlab("Region")
#수평 방향 그래프 (범주의 label이 길 때 용이)
tibble(state.region) |> 
  ggplot() + 
  geom_bar(aes(y= state.region)) +
  ylab("Region")

#도수분포가표가 자료인 경우-> geom_col()을 이용
#geom_col()은 따로 디폴트가 없음 
library(patchwork)
df_state_1 <- tibble(state.region) |> 
  count(state.region)
install.packages("patchwork")
df_state_1

#수직 , 수평 방향 그래프
bar1 <- ggplot(df_state_1, aes(x=state.region, y=n))+
  geom_col()+
  labs(x= NULL, y = NULL)
bar2 <- ggplot(df_state_1, aes(y=state.region, x=n))+
  geom_col()+
  labs(x= NULL, y = NULL)
bar1 + bar2

#상대도수로 막대그래프 작성 -> bar 그래프에서 after_stat(prop) 사용
#group = 1을 매핑 함수에 추가해야 상대도수별 막대그래프 생성 가능
tibble(state.region) |> 
  ggplot() +
  geom_bar(aes(x=state.region, y=after_stat(prop), group = 1))+
  labs(x="Region", y="Properties")
#상대도수 직접 변수 생성 후 그래프 작성
df_state_1_1 <- tibble(state.region) |>
  count(state.region) |> 
  mutate(p = n/sum(n))
df_state_1_1

ggplot(df_state_1_1) +
  geom_col(aes(x=state.region, y=p))+
  labs(x="Region", y="Properties")

#파이그래프 작성 -> coord_polar() 사용
tibble(state.region) |> 
  ggplot() + 
  geom_bar(aes(x="", fill=state.region))+
  labs(x=NULL, y=NULL)+
  coord_polar(theta = "y")+
  theme_void()

#percent()를 사용하여 %기호가 있는 백분율 변수 생성
library(scales)

df_state_2 <-
  tibble(state.region) |> 
  count(state.region) |> 
  mutate(pct = percent(n/sum(n)))
#확인 출력
df_state_2 |> 
  print()

#라벨로 추가하여 막대 그래프 생성
#geom_text 적용하려면 글로벌 매핑(ggpolot에 aes 함수 사용) 해야함
x<-df_state_2 |> 
  ggplot(aes(x="", y=n, fill=state.region)) +
  geom_col()+
  geom_text(aes(label = pct), size=5,
            position= position_stack(vjust=0.5))+
  labs(x = NULL, y= NULL, fill = NULL)
x
#라벨링한 막대그래프 파이그래프로 변환
x + 
  coord_polar(theta = "y")+
  theme_void()

#이변량 및 다변량 범주형 자료 탐색
#패키지에 데이터만 불러올때는 library 대신 data() 사용
data(Arthritis, package="vcd")
head(Arthritis)

#1차원 도수분포표 작성 : 함수 table()
my_table1 <- with(Arthritis, table(Improved))
my_table1

#1차원 상대도수분포표 작성 : 함수 prop.table()
#기존의 도수분포표를 그대로 넣으면 인자들이 상대도수로 변경됨
prop.table(my_table1)
#소수점 맞추기(option으로 조절)
options("digits" = 2)
prop.table(my_table1)

#2차원 분할표 작성
#2차원 분할표에서 확인해야할 점은 원 데이터에서 어떤 관계에 포커스를 두냐이다
my_table2 <- with(Arthritis, table(Treatment, Improved))
my_table2
#상대도수로 변환
prop.table(my_table2)

#조건변수에 따라 해석이 달라짐
#행 변수
prop.table(my_table2, 1)
#열 변수
prop.table(my_table2, 2)

#요인의 범주 합치기
Arthritis |> 
  count(Improved)

#factor()에 의한 통합
#factor로 모든 범주의 라벨을 바꾸어 통합하는 방법
#범주 개수가 적으면 사용 가능하나 개수가 많으면 사용하기 힘듦
Arthritis |> 
  mutate(Improved = factor(Improved,
                           labels = c("No", "Yes", "Yes")
                           )
         ) |> 
  count(Improved)

#함수 forcats::fct_recode()
#바꾸고자하는 범주의 level만 변경할 수 있음
Arthritis |> 
  mutate(Improved = fct_recode(Improved, No = "None", Yes = "Some",
                               Yes = "Marked")) |> 
  count(Improved)
