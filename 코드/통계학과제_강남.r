# 그래프를 사용하기 위한 ggplot2 라이브러리 불러오기 
library(ggplot2)

# p_value의 지수 표현을 경소수점으로 변
options(scipen = 999)

# CSV 파일에서 데이터 불러오기
sales_data <- read.csv("C:\\Users\\lovo6\\Desktop\\2023_2학기_수업\\머신러닝을위한통계학2\\sales_data.csv")

# 1. 강남지역 만을 대상을 매장 규모별로 매출액에 차이가 있는지 검정하라
# 귀무가설: 매장 크기에 따른 판매량 차이는 없을 것이다.
# 대립가설: 매장 크기에 따른 판매량 차이가 있을 것이다.

# position 변수가 gangnam인 행만 저장
sales_data_gangnam <- subset(sales_data, position == "gangnam")

# 일원분산분석 (종속: sales, 독립: scale(소형, 중형, 대형))
anova_result_task_1 <- aov(sales ~ scale, data = sales_data_gangnam)

# 분석 요약
summary(anova_result_task_1)
# scale의 p값이 0.0000000000000002로써 귀무가설을 기각하고 대립가설을 채택한다.
# 매장 규모별로 매출액에 차이가 있다.

ggplot(data = anova_result_task_1, aes(x = scale, y = sales)) + geom_boxplot()
# 박스 그래프를 이용해 시각적으로 표현할 시 결과가 명확히 나타난다.



# 2. 지역(강남, 강북)과 매장 규모에 따라 매출액에 차이가 있는지 검정하라.
# 귀무가설: 지역, 매장 규모에 따른 매출액의 차이는 없을 것이다.
# 대립가설: 지역, 매장 규모에 따른 매출액의 차이가 있을 것이다.

# 이원분산분석 (종속: sales, 독립: scale(소형, 중형, 대형), position(강남, 강북))
anova_result_task_2 <- aov(sales ~ scale * position, data = sales_data)

# 분석 요약
summary(anova_result_task_2)
# scale p값이 0.0000000000000002로써 scale의 수준에 따라 평균에 대한 차이가 발생
# position p값이  0.000516로써 position의 수준에 따라 평균에 대한 차이가 발생
# scale:position p값이 0.0000033로써 두 변수의 상호작용 효과를 확인 가능하다.

ggplot(anova_result_task_2, aes(x = scale, y = sales, fill = position)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sales by Store Size and Position", x = "Store Size", y = "Sales")
# 통계적인 검정으로는 가게 크기와 위치가 판매량에 유의미한 효과를 미치지만
# 그래프를 이용해 시각적으로 확인했을 때는 명확하게 나타나지 않는다.
