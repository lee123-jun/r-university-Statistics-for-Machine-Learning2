# vif 통계량 사용위한 선언  
install.packages("car")
library(car)

# pcor(), 편상관계수를 구하기 위한 선언
library(ppcor)

# Boston 데이터 불러오기 및 변수형 파악
library(MASS)
data <- Boston
str(data)

# 5개의 계량형 변수 추출
# medv: 주택가격, crim: 범죄율, zn: 비소오염지역, indus: 비소업 비율 지역, nox: 일산화탄소 농도
data_medv <- data$medv
data_crim <- data$crim
data_zn <- data$zn
data_indus <- data$indus
data_nox <- data$nox

# 문제 1: 선택한 변수에 대하여 다음의 그림처럼 산점도를 그려라.
pairs(data[,c("medv", "crim", "zn", "indus", "nox")])

# 문제 2: 변수들의 다중공선성에 대하여 평가하라
model <- lm(medv ~ crim + zn + indus + nox, data = data)
summary(model)
vif(model)

# 변수들을 데이터 프레임에 저장 (cor()인자는 최대 2개 변수)
values_df <- data.frame(medv = data_medv, crim = data_crim, zn = data_zn, indus = data_indus, nox = data_nox)

# 문제 3: 선택한 변수들간의 상관계수를 구하라.
cor_matrix <- cor(values_df)
print(cor_matrix)

# 문제 4: 선택한 변수간의 편상관계수를 구하고, 편상관계수에 대한 t검정 결과에 대하여 해석하라.(유의수준: 5%)
pcor(values_df)



                                    