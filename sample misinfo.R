install.packages("tidyverse")
library(tidyverse)
install.packages("dslabs")
library(dslabs)
data("heights")
head(heights)
mean(heights$height)
table(heights$sex)
heights %>% group_by(sex) %>% 
  summarise(mean= mean(height), sd = sd(height))
ggplot(heights, aes(sex, height))+
  geom_boxplot()
#From the boxplot, it is apparent that average female height
#is smaller than male. 
#To be more sure, let's run a t-test
t_model <- t.test(height~sex, data = heights)
t_model
#statistically significant difference in male and female
#mean heights found. 
