library(tidyverse)
#Importing data 
raw_data <- read_csv("covid_africa_data_untouched.csv")
head(raw_data)
glimpse(raw_data) 
renamed_data <- raw_data %>% 
  rename_with(.cols = 11:23, .fn = ~ paste0("q", 1:13)) %>%
  rename(familiarity = contains("Do you know anyone"))
names(renamed_data[,c(8,11:23)])
#reshaping variables
df1 <- renamed_data %>% mutate(gender = factor(Gender), 
         age_group = factor(`Age class`), 
         education = factor(`Highest level of education`), 
         familiarity = factor(familiarity)) %>%
  select(gender, age_group, education,  familiarity, q1:q13)

table(df1$age_group) 
# collapsing age class varible to three categories
levels(df1$age_group) <- list("Youths" = c("Less than 18 years", "18 - 24 years", "25 - 29 years"), 
                        "Adults" = c("30 - 34 years", "35 - 39 years", "40 - 49 years"), 
                        "Elderly" = c("50 - 59 years", "60 - 69 years", "70 years and above"))
sum(is.na(df1$age_group)) #checking missing values

table(df1$education)
levels(df1$education) <- list("Post_grad" =  "Post Graduate level", 
                              "College" = "Tertiary education",
                              "No_college" = c("No formal education", "Primary/Basic education", "Secondary/high school/form 4/5 education"))

table(df1$education)
sum(is.na(renamed_data$`Highest level of education`)) #checking na in raw data
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

#What's the distribution of misinformation score (q)?
ggplot(df, aes(q))+geom_histogram(color = "red", fill="blue")


fit <- lm(q~gender+age_group+education+familiarity, data = df)
summary(fit)

#check a suspect interaction
model1 <- lm(q~gender+age_group+education+familiarity+familiarity:education, data = df)
summary(model1)
#looks like there is not. 

require(gridExtra)
p1 <- df %>% ggplot(aes(x=education, y=q))+ 
 geom_boxplot()
p2 <- df %>% ggplot(aes(age_group, y=q))+geom_boxplot()
p3 <- ggplot(df, aes(gender, q,))+geom_boxplot()
p4 <- ggplot(df, aes(familiarity, q))+geom_boxplot()
grid.arrange(p1, p2, p3, p4, ncol =2)

#saving the finalized dataframe
write.csv(df, "africa_covid_regression.csv")

#some imaginary visualizations 
df2 <- df
age_img <- sample(18:70, size = 563, replace = TRUE)
age_img
summary(age_img)
df2$age <- age_img
head(df2)
df2 <- df2 %>% relocate(age, .before = q)
head(df2)
df2 %>% ggplot(aes(x=age, y = q, color = gender))+
  geom_point()+geom_smooth(method = "lm")

