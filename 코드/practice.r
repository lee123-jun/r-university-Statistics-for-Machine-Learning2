Telco_data <- read.csv("C:\\Users\\lovo6\\Desktop\\Telco-Customer-Churn.csv")
head(Telco_data)
str(Telco_data)

options(scipen = 9999)

# Churn 고객의 이탈여부 yes no 
# tenure 고객의 서비스 가입 기간
# MonthlyCharges 고객의 월 요금
# TotalCharges 고객이 사용하는 총서비스 요금

Telco_data$ Churn <- factor(Telco_data$Churn)
str(Telco_data)

glm_Churn <- glm(Churn~tenure+MonthlyCharges+TotalCharges, data = Telco_data, family=binomial)
summary(glm_Churn)