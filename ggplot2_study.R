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
#바꾸고자하는 범주의 level(속성명)만 변경할 수 있음 -> 범주 개수가 많을때 용이함
Arthritis |> 
  mutate(Improved = fct_recode(Improved, No = "None", Yes = "Some",
                               Yes = "Marked")) |> 
  count(Improved)

#분할표에서 결측값 처리
#airquality에서 월별 Ozone 값이 80이 넘는 날 수
with(airquality, table(OzHi = Ozone > 80, Month))

#결측값 범주도 포함해야함
#useNA = "ifany"를 사용하여 월별 결측값도 확인 가능
with(airquality, 
     table(OzHi = Ozone > 80, Month, useNA = "ifany"))


#이변량 및 다변량 범주형 자료를 위한 그래프
library(tidyverse)
data(Arthritis, package="vcd")
Arthritis |> 
  ggplot(aes(x = Treatment, fill = Improved)) +
  geom_bar()
#함수 count()를 사용할 경우 -> geom_col() 사용
Arthritis |> 
  count(Treatment, Improved) |> 
  ggplot(aes(x = Treatment, y = n, fill = Improved)) +
  geom_col()

#요인이 포함된 데이터 프레임인 경우
Arthritis |> 
  ggplot(aes(x = Treatment, fill = Improved)) +
  geom_bar(position = "dodge")
#count()로 작성된 도수분포가 자료인 경우
Arthritis |> 
  count(Treatment, Improved) |> 
  ggplot(aes(x = Treatment, y = n, fill = Improved))+
  geom_col(position = "dodge2")

#조건부 확률에 의한 막대그래프
Arthritis |> 
  ggplot(aes(x = Treatment, fill = Improved))+
  geom_bar(position = "fill")

#facet을 사용하여 Treatment 범주별로 그래프 분리
Arthritis |> 
  ggplot(aes(x = Improved, y = after_stat(prop),
             group = 1))+
  geom_bar()+
  facet_wrap(vars(Treatment))

#상대도수로 변경
#각 Treatment에 대한 비율을 구해야 하기 때문에
#group_by()로 Treatment의 범주별로 grouping
Arthritis |> 
  count(Treatment, Improved) |> 
  group_by(Treatment) |> 
  mutate(prop = n/sum(n)) |> 
  ggplot(aes(x = Improved, y = prop, fill = Improved))+
  geom_col()+
  facet_wrap(vars(Treatment)) +
  ylab(NULL)

#mosaic 그래프
#두개 이상의 범주형 변수 관계 탐색에 유용한 그래프
#조건이 추가됨에 따라 내부적으로 범주가 나누어지는 그래프
#조건이 너무 많으면 가시성이 확 떨어짐 (한 3개정도일때 사용하면 좋음)
library(vcd)
my_table <- with(Arthritis, table(Treatment, Improved))
mosaic(my_table, direction = "v")

#원자료를 입력한 공식적인 방법
# ~ 기준으로 왼쪽이 반응변수, 오른쪽이 설명변수
mosaic(Improved ~ Treatment, data = Arthritis, direction = "v")

#Titanic 데이터
#bullet이 있는 경우 해당 범주의 데이터 없음
str(Titanic)
#한개의 설명변수와 비교
mosaic(Survived ~ Class, data = Titanic, direction = "v")
mosaic(Survived ~ Sex, data = Titanic, direction = "v")
mosaic(Survived ~ Age, data = Titanic, direction = "v")
#두개의 설명변수와 비교
mosaic(Survived ~ Sex + Age, data = Titanic, direction = "v")
mosaic(Survived ~ Class + Sex, data = Titanic, direction = "v")
mosaic(Survived ~ Class + Age, data = Titanic, direction = "v")
#최종 그래프 비교
mosaic(Survived ~ Class + Sex + Age, data = Titanic, direction = "v")

#일변량 연속형 자료의 요약통계
#mean, median, mode, sd 등등의 통계는
#summarise()를 사용하여 출력 가능

#예제) UsingR 패키기의 데이터 프레임 cfb
data(cfb, package="UsingR")
str(cfb)
#INCOME의 요약 통계 확인
cfb |> 
  summarise(avg = mean(INCOME), med = median(INCOME))
#평균이 중앙값보다 훨씬 높음
#우측으로 치우친 분포임을 예측할 수 있ㅇ므
#그래프 작성
cfb |> 
  ggplot(aes(x = INCOME, y = after_stat(density)))+
  geom_histogram(bins = 35, fill = "steelblue")+
  geom_density(color = "red", linewidth = 1)
#우측으로 심하게 치우친 분포
#로그변환으로 좌우대칭에 가까운 분포 형태로 변환
cfb |> 
  mutate(log_income = log(INCOME)) |> 
  summarise(avg = mean(log_income),
            med = median(log_income))
#INCOME원자료에 0이 있는 경우 +1을 하여 로그변환
cfb |> 
  mutate(log_income = log(INCOME+1)) |> 
  summarise(avg = mean(log_income),
            med = median(log_income))
#변환 후 평균과 중앙값이 거의 유사함
#그래프 작성
cfb |> 
  mutate(log_income = log(INCOME + 1)) |> 
  ggplot(aes(x= log_income, y = after_stat(density)))+
  geom_histogram(color = "steelblue", bins = 35)+
  geom_density(color = "red", linewidth = 1)

#이변량 및 다변량 연속형 자료 탐색
mpg |> 
  count(cyl)
mpg_1<-mpg |> 
  filter(cyl != 5) |> 
  mutate(cyl = as.factor(cyl))

mpg_1 |> 
  ggplot(aes(x= hwy))+
  geom_histogram(binwidth = 5)+
  facet_wrap(vars(cyl), ncol = 1)
#position = "identity"를 사용하면 그래프가 겹쳐짐
mpg_1 |> 
  ggplot(aes(x = hwy, fill = cyl))+
  geom_histogram(binwidth = 5, alpha = 0.4,
                 position = "identity")

#나눠지는 변수는 범주형 or 문자형
mpg_1 |> 
  ggplot(aes(x = cyl , y = hwy))+
  geom_boxplot()+
  labs(x = "Number of Cylinders", y = "MPG")

#outliers 확인
pp <- ggplot(mpg_1, aes(x = cyl, y = hwy))+
  geom_boxplot()
pp_out<- ggplot_build(pp)[[1]][[1]]$outliers
pp_out
#3개의 boxplot에 대한 이상치가 나타남.
#이상값 자료 출력
mpg_1 |> 
  filter((cyl == 4 & hwy %in% pp_out[[1]])) |
  (cyl ==8 & hwy %in% pp_out[[3]])