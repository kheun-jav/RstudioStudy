#연습문제 7번
library(tidyverse)
data(homedata, package="UsingR")
str(homedata)
#(1)
h1<-homedata |> 
  pivot_longer(everything(), names_to ="year",
               values_to ="price") |> 
  mutate(year = str_remove_all(year, "y"))
h1 |> 
  ggplot(aes(x = year, y = price))+
  geom_boxplot()
h1 |> 
  ggplot(aes(x = year, y = price))+
  geom_boxplot() +
  geom_point(color = "red")

#(2)
homedata |> 
  mutate(up_down = if_else(y1970 < y2000, "집값 상승", "집값 하락")
  ) |> 
  select(up_down) |> 
  group_by(up_down) |> 
  count()

#8
library(GGally)
data(batting, package="UsingR")
str(batting)

#(1)
batting |> 
  select(AB,SO,HR) |> 
  ggpairs(upper = list(continuous = "density"))
#HR, SO 변수가 오른쪽으로 긴 꼬리를 가지고 있다 
#-> 분산 안정화를 위한 로그 변환이 필요해 보인다

#(2)
batting_log <- 
  batting |> 
  mutate(HR = log(HR), SO = log(SO))

batting_log |> 
  select(AB,SO,HR) |> 
  ggpairs(upper = list(continuous = "density"))

#(3)
batting|> 
  ggplot(aes(x = SO, y = HR))+ 
  geom_jitter()+
  geom_text(data = batting |> slice_max(HR/AB, n = 1),
            aes(label = playerID),
            vjust = "buttom", hjust = "right")
#최대 타율을 가진 플레이어의 정보를 구하기 위해 
#데이터에서 slice_max()를 사용

#(4)
#삼진을 많이 당할수록 홈런을 많이 친다는 해석보다,
#삼진들 많이 당할수록 타석에 오른 횟수가 많고,
#그로 인해 홈런수도 많다고 해석하는게 옳다.
tibble(batting) |> 
  slice_max(HR, n = 10) |> 
  select(playerID, G, SO)

#9
str(iris)

#(1)
iris |> 
  ggpairs(aes(color = Species))

#(2)
iris |> 
  pivot_longer(c(Sepal.Length,Sepal.Width), names_to = "Sepal",
               values_to = "Size") |> 
  mutate(Sepal = str_remove_all(Sepal, "Sepal.")) |> 
  ggplot(aes(x = Species, y = Size))+
  geom_boxplot()+
  facet_wrap(vars(Sepal))+
  labs(y = NULL, title = "Sepal Length and Sepal Width")
#Sepal.Length는 대체적으로 virginica, versicolor, setosa 순으로 길고,
#virginica에서 극단적으로 낮은 값 1개가 관측된다
#Sepal.Width는 대체적으로 setosa, virginica, versicolor 순으로 크고,
#setosa에서 이상치 2개, virginica에서 이상치 2개가 관측된다.

#10
data(barley, package = "lattice")

str(barley)
#모든 변수가 범주형인 자료

#(1)
barley |> 
  ggplot(aes(y = variety, x = yield))+
  geom_boxplot()
