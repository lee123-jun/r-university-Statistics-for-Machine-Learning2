# 추출할 데이터를 변수에 저장
# help(iris)
# head(iris, 5)
# str(iris)

# x[1:3] x란 벡터에서 앞에서 3번째까지만 추출
# $ 표시를 이용해서 원하는 데이터 접근 ? 

iris$Petal.Length[1:50]
iris$Petal.Length[iris$Species == "Versicolor"]

s_length <- iris$Petal.Length[1:50]
v_length <- iris$Petal.Length[iris$Species == "Versicolor"]

t.test(s_length, v_length)