# Title and Header --------------------------------------------------------
##Title: COVID-19 Death and the role of age
##Author: Danladi Nengak Precious
##Date: 18/Nov/2024




##Covid-19 has had disastrous effect and one of the worst outbreak in human history.
##Data have been collected of it impact for a while now, and we want to study
#the role of age in infection, recovery and death
# Installing and loading packages -----------------------------------------

pacman::p_load(
  rio,                 #data import and export
  tidyverse,           #data management and visualisation
  rstatix,             #for summary statistics
  janitor,             #data cleaning
  gtsummary,           #For interactive tables
  epikit,              #for age categoorization
  skimr                #data exploration
)


# Data importation --------------------------------------------------------



COVID19 <- read_csv("C:/Users/User/Desktop/COVID 19/COVID19_line_list_data.csv")
View(COVID19_line_list_data)


# Data Exploration --------------------------------------------------------

summary(COVID19)



# Data cleaning and manipulation ------------------------------------------

##Our death column has 0 as the person recovered and 1 if the person died
##However some of the data used the date of death and hence the need to clean it

death1 <- COVID19 %>% 
  mutate(death = as.integer(if_else(death != 0, 1, 0))) #Converting date varirables to integers and replacing them with 1
view(death1)


# Descriptive Statistics --------------------------------------------------

##Mean dead rate and survival

death_rate <- death1 %>% 
  summarise(death_rate = (sum(death)/n()) *100) %>% 
              view() ##Recovery to death rate is 5.8%


# Data Visualization ------------------------------------------------------


##Ratio of male to female who recovered and died

death1 %>% 
  filter(!is.na(death)) %>% 
  ggplot(mapping = aes(x = gender, 
                       fill = as.factor(death)))+
  geom_bar() +
  labs(x = "Gender", y = "Count", title = "A Bar chart of Death and recovery rate by gender")+
  scale_fill_manual(values = c("0" = "darkgreen", "1" = "darkred"),
                    labels = c ("0" = "recovered", "1" = "death"),
                    name = "Death status")

 ## Comparing recovery by age category
death2 <- death1 %>% 
  mutate(age_category = case_when(age >= 0 & age <=10 ~ "Child",
                                      age >10 & age <= 20 ~ "Teen",
                                      age > 20 & age <= 30 ~ "Young adult",
                                      age > 30 & age <= 40 ~ "Adult", 
                                      age > 40 & age <= 50 ~ "Elderly", 
                                      age > 50 ~ "50+" )) %>% 
  view()

death2 %>% 
  filter(!is.na(age_category)) %>% 
  ggplot(mapping = aes(x = age_category,
                       fill = as.factor(death)))+
  geom_bar()+
  labs(x = "Age Category", 
       y= "Count",
       title = "Date and recovery rate") +
  scale_fill_manual( values = c( "0" = "darkgreen", "1" = "darkred"),
                     labels = c( "0" = "Recovered",
                                 "1" = "Died"),
                     name = "Death Status")


# Statistical analysis ----------------------------------------------------
##Hypothesis testing
#Research question: Is there a difference in mean age of recovery and mean age of death rate?
#H0: There is no difference in the mean age of those who recovered and those who died
#H1: there is a difference in the mean age of the who recovered and the mean age of those who died

death2 %>% ##Recovery rate
  summarise(recovery_rate = mean(death) * 100)

##Is there a difference in the mean age of  those who recovered and the mean age of those who died
dead <- death2 %>%                 ##Creating a subset of data of death individuals
  filter(death == 1) %>% 
  view()

alive <- death2 %>%               ##Creating a subset of data of individuals who recovered
  filter(death == 0) %>% 
  view()

mean(dead$age, na.rm = TRUE) #the mean age of those who died
mean(alive$age, na.rm = TRUE) #the mean age of those who recovered

##Comment: There us an observe difference in the mean age of those who recovered and the mean age of those who died
##Is this difference statistically significant

t.test(dead$age, alive$age,
       alternative = "two.sided",
       conf.level = 0.95)
              ##P < 0.05

   ##With this we reject the Null hypothesis and accept the alternate hypothesis.

