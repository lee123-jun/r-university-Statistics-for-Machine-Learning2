# data.frame = mtcars
# gear, cyl -> 기어타입과 실린더 수 간의 유의미한 관계가 있는가?
# 카이제곱 chisq.test()
mtcars_gear <- mtcars$gear
mtcars_cyl <- mtcars$cyl

x <- table(mtcars$gear, mtcars$cyl)
chisq.test(x)

