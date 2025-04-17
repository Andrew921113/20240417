df <- read.csv("C:/Users/user/Downloads/Mobiles.csv")

#Q1
Apple <- subset(df,Company == "Apple")
Samsung <- subset(df,Company == "Samsung")

sd(Apple$PriceUSA)
median(Apple$PriceUSA)

#Apple美國售價中位數為999，標準差為247.8969

sd(Samsung$PriceUSA)
median(Samsung$PriceUSA)

#Samsung美國售價中位數為699，標準差為515.3826

#Q2
boxplot(PriceChina~Year, data = df,   
        xlab = "年份", ylab = "中國售價",
        main = "年份-中國售價之盒鬚圖")

install.packages("dplyr")
library(dplyr)

install.packages("ggplot2")
library(ggplot2)

g1 <- ggplot(df, aes(x=PriceChina)) + geom_histogram()
g2 <- g1 +ggtitle("中國售價長條圖")+xlab("中國售價")+ylab("數量") ;g2

#Q3
g3 <- ggplot(df, aes(x = Weight, y = Battery))+ geom_point()+
  xlab("手機重量")+ylab("電池容量");g3

#Q4
leveneTest(PriceUSA~factor(Company),df)
#Pr(>F) = 5.218e-14 < 0.05 故不同廠商的美國售價之變異數是有所差異的

#Q5
Q5 <- aov(Weight~factor(Company),data = df ) 
summary(Q5)
#Pr(>F) = 3.55e-05 < 0.05 ，因此可以說各廠牌的手機重量之平均數有顯著差異。

#Q6
Q6 <- aov(Weight~factor(Company)+factor(Battery),data = df)        
summary(Q6)
#兩者的Pr(>F)皆小於2e-16，即小於 0.05，故皆有顯著影響

#Q7 
Q7 <- subset(df,Year == "2024")
prop.table(table(Q7$Company))
#Apple 的比例為0.12 

#Q8
names(df)=c("company", "name", "weight", "battery", "price.c", "price.u", "year")
df = df %>% mutate(price.t = price.u*32)
write.csv(df,file="B112040008.csv",row.names = T)
