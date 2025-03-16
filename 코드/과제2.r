#과제 2

#Mass Package 내부의 mtcars data set 불러오기
library(MASS)

# p_value의 지수 표현을 소수점으로 변경
options(scipen = 999)

#mtcars data set을 data 변수에 대입
data <- mtcars 

#데이터 출력
print(data)

# cor함수를 이용해 상관계수 행렬 계산
cor_matrix=cor(data)

#문제 1: 11개 특성들 간의 상관계수 행렬을 구하시오
print(cor_matrix)



# 문제 2: 자동차 연비를 나타내는 mpg와 상관관계가 가장 높은 특성 3가지는 무엇이며, 이들의 상관계수는?

# 다른 모든 특성들과 mpg의 상관관계
cor_with_mpg <- cor_matrix[,"mpg"]

# 내림차순으로 정렬
sorted_cor <- sort(cor_with_mpg, decreasing = T)  

# mpg(본인)를 제외한 상위 3개의 특성이 상관관계가 가장 높다
top_three <- sorted_cor[2:4]

#상위 3개의 특성 출력
print(top_three)



#문제 3: 수동 변속기 여부(am)에 따라 자동차 연비(mpg)에 차이가 있다고 할 수 있는가?
#수동변속기(am): 0,1 범주형, 연비(mpg): 연속형 / t-test
#귀무가설: 수동 변속기 여부(am)에 따른 자동차 연비(mpg)는 차이가 없다.
#대립가설: 수동 변속기 여부(am)에 따른 자동차 연비(mpg)는 차이가 있다.
#group0 -> am가 0인 부동산에 대한 mpg의 값
#group1 -> am가 1인 부동산에 대한 mpg의 값
group0 <- data$mpg[data$am == 0]
group1 <- data$mpg[data$am == 1]

#평균 비교를 위한 T-검정
t_test_result <- t.test(group0, group1)

#p_vaule 변수에 값 대입
p_value <- t_test_result$p.value
print(p_value)

print(p_value)
#p_value 값이 0.05보다 작기 때문에 귀무가설을 기각하고 대립가설을 채택한다. 
#즉 수동 변속기 여부에 따른 자동차 연비는 차이가 있다.



#문제 4: 기어 수(gear)에 따라 쿼터마일시간(qsec)이 달라진다고 할 수 있는가?
#기어(gear): 범주형(3가지)->anova 분석 필요, 쿼터마일시간(qsec): 연속형 / 상관계수, 선형회귀
#귀무가설: 기어(gear)에 따른 쿼터마일시간(qsec)은 차이가 없다.
#대립가설: 기어(gear)에 따른 쿼터마일시간(qsec)은 차이가 있다.

#두 특성의 상관계수 계산, 관계를 이해
correlation_nox_rm <- cor(data$gear, data$qsec)

# 선형회귀 모델 
model <- lm(qsec ~ gear, data = data)

# 회귀분석의 요약 및 기어 수(gear)의 p값 
model_summary_four <- summary(model)
p_value_gear <- model_summary_four$coefficients["gear", "Pr(>|t|)"]

print(p_value_gear)
#p_value 값이 0.05보다 높기 때문에 귀무가설을 채택한다.
#즉 기어에 따른 쿼터마일시간은 차이가 없다.



#문제 5: 기어 수(gear)에 따라 자동차 연비(mpg)에 차이가 있다고 할 수 있는가?
#기어(gear): 범주형, 자동차 연비(mpg): 연속형 / t-test
#귀무가설: 기어(gear)에 따른 자동차 연비(mpg)은 차이가 없다.
#대립가설: 기어(gear)에 따른 자동차 연비(mpg)은 차이가 있다.
#group0 -> gear가 3인 자동차 연비의 값
#group1 -> gear가 4인 자동차 연비의 값
#gear의 데이터는 3,4만 존재 int type
group0 <- data$mpg[data$gear == 3]
group1 <- data$mpg[data$gear == 4]

#평균 비교를 위한 T-검정
t_test_result <- t.test(group0, group1)

#p_vaule 변수에 값 대입
p_value <- t_test_result$p.value

print(p_value)
#p_value 값이 0.05보다 작기 때문에 귀무가설을 기각하고 대립가설을 채택한다.
#즉 기어에 따른 자동차연비는 차이가 있다.
