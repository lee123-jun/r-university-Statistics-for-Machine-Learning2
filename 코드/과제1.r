#과제 1
# 시각화 필수 
?Boston
#Mass Package 내부의 Boston data set 불러오기
library(MASS)

# p_value의 지수 표현을 소수점으로 변경
options(scipen = 999)

#Boston data set을 data 변수에 대입
data <- Boston

# 데이터 출력
print(data)

# cor함수를 이용해 상관계수 행렬 계산
cor_matrix=cor(data) 

#문제 1번: 4개 특성들 간의 상관계수 행렬을 구하시오
print(cor_matrix)


#문제 2번: medv와 상관관계가 가장 높은 특성 3가지 and 이들의 상관계수는?

# 다른 모든 특성들과 medv의 상관관계
cor_with_medv <- cor_matrix[,"medv"]

print(cor_with_medv)

# 내림차순으로 정렬
sorted_cor <- sort(cor_with_medv, decreasing = T)  

# medv(본인)를 제외한 상위 3개의 특성이 상관관계가 가장 높다
top_three <- sorted_cor[2:4]

# 상위 3개의 특성 출력
print(top_three)



#문제 3: 찰스 강과의 경계(chas)에 따라 주택 가격(medv)에 차이가 있다고 할 수 있는가?
#찰스강: 0,1 집단이 2개 범주형, 주택 가격: 연속형 / t-test 사용
#귀무가설: 찰스 강의 경계에 따른 주택 가격에 차이는 없다.
#대립가설: 찰스 강의 경계에 따른 주택 가격에 차이는 있다.

#group0 -> chas가 0인 부동산에 대한 medv의 값
#group1 -> chas가 1인 부동산에 대한 medv의 값
#chas의 데이터는 0,1만 존재 int type
group0 <- data$medv[data$chas == 0]
group1 <- data$medv[data$chas == 1]

#평균 비교를 위한 T-검정
t_test_result <- t.test(group0, group1)

#p_vaule 변수에 값 대입
p_value <- t_test_result$p.value
print(p_value)

print(p_value)
#p_value 값이 0.05보다 작기 때문에 귀무가설을 기각하고 대립가설을 채택한다. 
#즉 강 경계에 따른 주택 가격 차이는 존재한다.



#문제 4: 일산화질소농도(nox)와 평균 방 수(rm) 간에는 어떤 관계가 있으며, nox에 따라 rm 이 달라진다고 할 수 있는가
#둘다 계량치이기 때문에 검정할 수 없다. -> 둘다 계량형이면 상관분석을 해야한다
#정성데이터->text data , 정량데이터->수치 데이터 / 계량->연속, 계수->이산 
#일산화질소농도: 연속형, 방 수: 연속형 / 상관계수, 선형회귀분석
#귀무가설: 일산화질소농도(nox)와 평균 방 수(rm) 간에는 관계가 없다.
#대립가설: 일산화질소농도(nox)와 평균 방 수(rm) 간에는 관계가 있다.

#두 특성의 상관계수 계산, 관계를 이해
correlation_nox_rm <- cor(data$nox, data$rm)

# 선형회귀 모델
model <- lm(rm ~ nox, data = data)

# 회귀분석의 요약 및 일산화질소농도(nox)의 p값 
model_summary_four <- summary(model)
p_value_nox <- model_summary_four$coefficients["nox", "Pr(>|t|)"]

print(p_value_nox)
#p_value 값이 0.05보다 작기 때문에 귀무가설을 기각하고 대립가설을 채택한다. 
#즉 일산화질소농도(nox)와 평균 방 수(rm) 간에는 관계가 있다(달라진다 볼 수 있다).



#문제 5: 문제 2에서의 주택가격과 높은 상관관계를 갖는 3개의 변수로 회귀식을 구하라.

# 2번의 상관관계가 높은 상위 3개의 변수(rm, zn, black) 회귀 모델 구축
# 종속: medv / 독립: rm, zn, black
model <- lm(medv ~ rm + zn + black, data = data)

plot(Boston$zn ~ Boston$rm, data=Boston)

# 모델 요약
model_summary_five <- summary(model)

# 회귀식: medv = -37.914579 + 8.206927*rm + 0.048989*zn + 0.023307*black
