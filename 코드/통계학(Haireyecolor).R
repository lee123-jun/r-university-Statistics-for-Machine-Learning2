data.frame = HairEyeColor
str(HairEyeColor)

# 눈색 vs 머리색 / 관계
# 범주형 데이터로 만들어야 한다
# table 함수 사용 !

x <- table(HairEyeColor$Eye, HairEyeColor$Hair)


