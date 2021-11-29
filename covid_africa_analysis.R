library(tidyverse)
#Importing data 
library(readxl)
cad <- read_excel("covid_africa_data.xlsx")
head(cad)

#reshaping variables
df1 <- cad %>% mutate(gender = factor(Gender), 
         age_group = factor(`Age class`), 
         education = factor(`Highest level of education`), 
         familiarity = factor(Know_a_covid_patient)) %>%
  select(gender, age_group, education,  familiarity, q1:q13)

table(cad$`Age class`)
table(df1$age_group)
# collapsing age class varible to three categories
levels(df1$age_group) <- list("Youths" = c("Less than 18 years", "18 - 24 years", "25 - 29 years"), 
                        "Adults" = c("30 - 34 years", "35 - 39 years", "40 - 49 years"), 
                        "Elderly" = c("50 - 59 years", "60 - 69 years", "70 years and above"))
sum(is.na(df1$age_group)) #checking missing values

levels(df1$education) <- list("college_educated" = c("Tertiary education", "Post Graduate level"), 
                              "No_college" = c("No formal education", "Primary/Basic education", "Secondary/high school/form 4/5 education"))

sum(is.na(cad$`Highest level of education`))
sum(is.na(df1$education))

df1[df1 == "Strongly Disagree"] <- "0"
df1[df1 == "Disagree"] <- "0"
df1[df1 == "Neutral"] <- "1"
df1[df1=="Agree"] <- "4"
df1[df1=="Strongly Agree"] <- "4"


head(df1)
class(df1$q1)
df1[, 5:17] <- lapply(df1[, 5:17], as.numeric)
class(df1$q1)
head(df1)
df1$q <- rowMeans(subset(df1, select = q1:q13), na.rm = T)
head(df1)

df <- df1 %>% select(gender, age_group, education, familiarity, q )
head(df)



model <- lm(q~gender+age_group+education+familiarity, data = df)
summary(model)

require(gridExtra)
p1 <- df %>% ggplot(aes(x=education, y=q))+ 
 geom_boxplot()
p2 <- df %>% ggplot(aes(age_group, y=q))+geom_boxplot()
p3 <- ggplot(df, aes(gender, q,))+geom_boxplot()
p4 <- ggplot(df, aes(familiarity, q))+geom_boxplot()
grid.arrange(p1, p2, p3, p4, ncol =2)
