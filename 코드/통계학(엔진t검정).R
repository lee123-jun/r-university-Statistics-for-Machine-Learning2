data.frame = mtcars
mpg <- mtcars$mpg

# 4기통 엔진 연비값
group_4 <- mpg[mtcars$cyl == 4]
# 8기통 엔진 연비값
group_8 <- mpg[mtcars$cyl == 8]
# alternative -> "less" or "greater" 비교 안쓰면 같냐 다르냐만 나온다.
# T검정, 카이제곱 검정

# 카이제곱 검정 chisq.test
# 독립변수 종속변수 == 범주형 데이터일때
t.test(group_4, group_8, alternative = "greater")
