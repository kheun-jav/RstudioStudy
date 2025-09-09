#UsingR::grades
#122명의 학생들이 선수과목과 후수과목으로 강좌에서 받은 학점 정보
data(grades, package="UsingR")
str(grades)

#(1) 데이터프레임의 테이블 작성
with(grades, table(prev, grade))

#(2) 범주 통합하여 재조정
#나의 해답(라벨 각자 붙였음 -> 불필요한 작업 반복)
x<-grades |> 
  mutate(grade.rec = factor(grade,
                           labels = c("A", "A", "B", "B", "B", "C", "C", "Fail", "Fail")),
         prev.rec = factor(prev,
                            labels = c("A", "A", "B", "B", "B", "C", "C", "Fail", "Fail")))
tab1<-with(x, table(prev.rec, grade.rec))
tab1
prop.table(tab1, 1)
mosaic(prev.rec ~ grade.rec, x, direction="v")
#교수님 해답(라벨을 rep로 따로 만들었음)
new_grade <- c(rep("A", 2), rep("B", 3), rep("C", 2), rep("Fail", 2))
new_grade

grades_1<- grades |> 
  mutate(prev.rec = factor(prev, labels= new_grade),
         grade.rec = factor(grade, labels=new_grade))
grades_1
tab2 <- with(grades_1, table(prev.rec, grade.rec))
tab2
options("digits" = 2)
prop.table(tab2, 1)

library(vcd)
mosaic(prev.rec ~ grade.rec, grades_1, direction="v")
