# 일원분산분석 = aov()
# A,B,C 3반의 수학점수를 비교
# 종속변수 1개, 독립변수 3개 이상일 때
# 3그룹 이상의 평균을 비교할 때 사용 / 3종 == 3그룹
# aov(종속변수 ~ 독립변수, 데이터)
# 독립변수: 종 , 종속변수: Sepal.Width
# 독립성, 정규성, 등분산성(barlett.test)
# 귀무가설: 3그룹의 평균이 같다.

bartlett.test(Sepal.Width ~ Species, data = iris)

iris_aov <- aov(iris$Sepal.Length ~ Species, data = iris)
summary(iris_aov)
