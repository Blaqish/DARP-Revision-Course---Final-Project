
# Title -------------------------------------------------------------------

#Nengak D.P.
#Hands on



# Installing Packages -----------------------------------------------------

if(!require("pacman"))install.packages("pacman")
pacman::p_load(janitor,
               skimr,
               rstatix,
               tidyverse, 
               rio)


# Importing Dataset -------------------------------------------------------

data("infert")
view(infert)





# Data description and summary --------------------------------------------
Min_age = min(infert$age, na.rm = TRUE)
view(Min_age)

Max_age = max(infert$age, na.rm =TRUE)
view(Max_age)

infert1 <- infert %>% 
  mutate(age = case_when(age =21 & age <25 ~ "21-24", 
         age >=25 & age <30 ~ "25- 29", 
         age >=30 & age <35 ~ "30-34", 
         age >=35 & age <40 ~ "35-39",
         age >=40 & age <45 ~ "40 - 44"))

view(infert1)



#Age demographics

Age_distribution <- infert1 %>% 
  group_by(age) %>% 
  summarise(count = n()) %>% 
  mutate(percentage = count/sum(count)*100) %>% 
  tibble()

view(Age_distribution)

#Education demographics
Education <- infert1 %>% 
  group_by(education) %>% 
  summarise(count = n()) %>% 
  mutate(percentage = count/sum(count)*100)

view(Education)

#Case and Control
caseControl <- infert1 %>% 
  group_by(case) %>% 
  mutate(case = case_when(case == 1 ~ "case", case == 0 ~ "control")) %>% 
  summarise(count = n()) %>% 
  mutate(percentage = count/sum(count)*100)

view(caseControl)



# Data Description and Summary --------------------------------------------


summary(infert1)
str(infert1)



# Data Viz ----------------------------------------------------------------
infert1 %>% 
group_by(case, spontaneous, induced) %>% 
  mutate(case = case_when(case == 1 ~ "case", case == 0 ~ "control")) %>% 
ggplot(aes(x = case))+
  geom_bar()
