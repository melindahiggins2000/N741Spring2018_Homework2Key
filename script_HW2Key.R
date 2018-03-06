library(car)
library(tidyverse)

#' Question 1: What kind of R object 
#' is the Davis dataset?

str(car::Davis)
class(car::Davis)

#' Question 2: How many observations 
#' are in the Davis dataset?

dim(car::Davis)

# dplyr approach
car::Davis %>%
  summarise(n = n())

#' Question 3: For reported weight, 
#' how many observations have a missing value?

length(car::Davis$weight)
sum(is.na(car::Davis$weight))
summary(car::Davis$weight)

summary(car::Davis$repwt)
summary(is.na(car::Davis$repwt))

# dplyr approach
car::Davis %>%
  summarise_all(funs(sum(is.na(.))))

# purrr approach
car::Davis %>% map(~ sum(is.na(.)))

#' Question 4: How many observations 
#' have no missing values? (HINT: find complete cases)

nomiss <- complete.cases(car::Davis)
sum(nomiss)

# Create a subset containing only females.
#' Question 5: How many females 
#' are in this subset?

davisFonly <- car::Davis %>%
  filter(sex == "F")

### BMI Categories
# Category    | BMI range (kg/m2)   
# ----------- | -------------------
# Underweight | <18.5              
# Normal      | 18.5 to <25              
# Overweight  | 25 to <30                
# Obese       | 30 or higher    

# Question 6: What is the average BMI 
# for these individuals?

Davis <- car::Davis %>%
  mutate(bmi = weight/(height/100)^2) 

mean(Davis$bmi)

Davis %>%
  summarise(meanbmi = mean(bmi))

# Question 7: How do these individuals 
# fall into the BMI categories (what are 
# the frequencies and relative %'s)?

Davis <- Davis %>%
  mutate(bmicat = if_else(bmi<18.5,
                        "1. underweight",
                        if_else(bmi<25,
                                "2. normal",
                                if_else(bmi<30,
                                        "3. overweight",
                                        "4. obese",
                                        "missing"),
                                "missing"),
                        "missing"))

class(Davis$bmicat)
table(Davis$bmicat)
table(as.factor(Davis$bmicat))
summary(as.factor(Davis$bmicat))

Davis %>% count(bmicat)

mtcars %>% count(cyl)

gmodels::CrossTable(x=Davis$bmicat, y=Davis$sex)

library(janitor)
Davis %>%
  tabyl(bmicat)

t1 <- Davis %>%
  tabyl(bmicat)

knitr::kable(t1)

# create some figures

# histogram of bmi
# there is an outlier - update plot or 
# data to exclude outlier in plot

Davis2 <- Davis %>%
  filter(bmi < 100)

ggplot(Davis2, aes(bmi)) +
  geom_histogram()

Davis2 %>%
  ggplot(aes(bmi)) +
  geom_histogram(aes(y=..density..),
                 colour="black",fill="yellow",
                 binwidth=1) +
  geom_density(alpha=.2, fill="blue")

# boxplots of bmi by gender

Davis2 %>% ggplot(aes(x=sex, y=bmi)) +
  geom_boxplot()

# cluster barchart of bmi categories by gender

Davis2 %>% ggplot(aes(x=bmicat, fill=sex)) +
  geom_bar(position = "dodge")
