
# Title -------------------------------------------------------------------
##Author: Danladi N.P
##Date: 23rd Sept, 2024
##email: danladinengakprecious@gmail.com


---
  title: "DARP Revision Course - Final Project"



# Installing and loading Packages -----------------------------------------
if(!require("pacman"))install.packages("pacman")
pacman::p_load(rio,         # for data import and export
               janitor,     # for data cleaning
               rstatix,     # for summary statistics
               skimr,       # for data exploration
               gtsummary,   # for interactive frequency tables
               epikit,      # for age categorization
               tidyverse)   # for data management and visualization


Malnutrition <- import("malnutrition_data.xlsx") #Importing data and renaming it to malnutrition
View(Malnutrition)



# Data Exploration --------------------------------------------------------
dim(Malnutrition) #Exploring the data dimension 
head(Malnutrition) # Exploring the first rows
tail(Malnutrition) # Exploring the last 6 rows


unique(Malnutrition$`Quality of mother's diet`) # Exploring Unique variables in Quality of Mothers diet
unique(Malnutrition$`Mother's Education`) # Exploring unique values in Mother's Education
unique(Malnutrition$`Breastfeeding Status`) # Unique values in Breastfeeding Status
unique(Malnutrition$`Child's gender`) # Unique values in Child's gender
unique(Malnutrition$`Child's nutritional status`) # Unique values in Child's Nutritional Status
unique(Malnutrition$`Child's immunization status`) # Unique values in Child's Nutritional Status
unique(Malnutrition$`Child's immunization status`) # Unique values in Child's immunization status
unique(Malnutrition$`Child's diarrhoea incidence`) # Unique values in Child's diarrhea incidence
unique(Malnutrition$`Access to healthcaare services`) # Unique values in Access to healthcare.
## There is a decent number of inconsistencies in the data and it is necessary we clean it before our analysis

summary(Malnutrition) # observing for data summary





# Data Cleaning and Manipulation ------------------------------------------

#first let us clean our column name to have standardized names

Malnutrition_clean1 <-  clean_names(Malnutrition)

#We will use the tbl_summary to get a clear view of our data summary
Malnutrition_clean1 %>% 
  tbl_summary() %>%  #A summary table of the data
  bold_labels()


#Cleaning the data based ob observed erros
Malnutrition_clean2 <- Malnutrition_clean1 %>% 
  #Cleaning the Quality of Mother's diet variable
  mutate(quality_of_mothers_diet = recode(quality_of_mothers_diet,
                                          #Old name = New name
                                          "average" = "Average",
                                          "avirage" = "Average",
                                          "avverag" = "Average",
                                          "God" = "Good",
                                          "good" = "Good",
                                          "goood" = "Good",
                                          "poor" = "Poor",
                                          "porr" = "Poor",
                                          "pur" = "Poor")) %>% 
  #Cleaning Mother's Education Variable
  mutate(mothers_education = recode(mothers_education, 
                                    "no education" = "No Education",
                                    "primary" = "Primary Education",
                                    "Primary" = "Primary Education",
                                    "primry" = "Primary Education",
                                    "Promry" = "Primary Education",
                                    "secndary" = "Secondary Education",
                                    "secondary" = "Secondary Education",
                                    "teriary" = "Tertiary Education",
                                    "tertiary" = "Tertiary Education",
                                    "tetiay" = "Tertiary Education")) %>%
  #Cleaning the Breastfeeding Status Variable
  mutate(breastfeeding_status = recode(breastfeeding_status,
                                       "exclusively" = "Exclusively",
                                       "not breastfed" = "Not Breastfed",
                                       "partially" = "Partially")) %>% 
  #Cleaning the Child's gender Varaible
  mutate(childs_gender = recode(childs_gender,
                                "F" = "Female",
                                "Family" = "Female",
                                "femaale" = "Female",
                                "female" = "Female",
                                "M" = "Male",
                                "male" = "Male",
                                "Mall" = "Male")) %>% 
  #Cleaning the Child's Nutritional Status Variable
  mutate(childs_nutritional_status = recode(childs_nutritional_status,
                                            "Norm" = "Normal",
                                            "normal" = "Normal",
                                            "Ovaweigh" = "Overweight",
                                            "overweight" = "Overweight",
                                            "Undaweigh" = "Underweight",
                                            "underweight" = "Underweight")) %>% 
  #with our clean data, we are now manipulating new columns
  #Creating a new column called mothers_age_category: 
  #with age 18 to 25 as young mothers, age 26 to 35 as middle age 
  #mothers and greater than 35 as senior mothers.
  mutate(mothers_age_category = case_when(mothers_age >= 18 & mothers_age <= 25 ~ "Young Mothers",
                                          mothers_age >= 26 & mothers_age <= 35 ~ "Middle Age",
                                          mothers_age > 35 ~ "Senior Mothers",)) %>% 
  #Rearranging the Age category to have "Young mothers first, then Middle Age and Senior mothers"
  mutate(mothers_age_category = fct_relevel(mothers_age_category,
                                            "Young Mothers",
                                            "Middle Age",
                                            "Senior Mothers")) %>% 
  #Creating a new column called BMI category from mother_bmi columns: 
  #with bmi less than 18.5 as underweight, 
  #bmi between 18.5 and 24.9 as normal weight, 
  #bmi between 25.0 and 29.9 as overweight, and bmi greater than 30.0 as obesity.
  mutate(bmi_category = case_when(mother_bmi <18.5 ~ "Underweight",
                                  mother_bmi >= 18.5 & mother_bmi <= 24.9 ~ "Normal weight",
                                  mother_bmi >= 25.0 & mother_bmi <= 29.9 ~ "Overweight",
                                  mother_bmi >= 30 ~ "Obesity")) %>% 
  #Rearranging variables in the order of "Underweight, Normal Weight, Overweight, Obesity and Unknown"
  mutate(bmi_category = fct_relevel(bmi_category,
                                    "Underweight",
                                    "Normal weight",
                                    "Overweight",
                                    "Obesity")) %>% 
  #Re-leveling
  #1. Quality of mother’s diet as – poor, average, good
  mutate(quality_of_mothers_diet = fct_relevel(quality_of_mothers_diet,
                                               "Poor",
                                               "Average",
                                               "Good")) %>% 
  #2.	Mother’s education as – no education, primary, secondary, tertiary
  mutate(mothers_education = fct_relevel(mothers_education,
                                         "Primary Education",
                                         "Secondary Education",
                                         "Tertiary Education",
                                         "No Education")) %>% 
  #3.	Breastfeeding status as – not breastfed, partially, exclusively
  mutate(breastfeeding_status = fct_relevel(breastfeeding_status,
                                            "Not Breastfed",
                                            "Partially",
                                            "Exclusively")) %>% 
  #4.	Child’s nutritional status as – underweight, normal, overweight.
  mutate(childs_nutritional_status = fct_relevel(childs_nutritional_status,
                                                 "Underweight",
                                                 "Normal",
                                                 "Overweight")) %>% 
  select(mothers_age, mothers_age_category, mother_bmi, bmi_category,
         mothers_education, quality_of_mothers_diet, mothers_haemoglobin,
         breastfeeding_status, household_income, childs_gender, childs_age, 
         childs_height, childs_weight, childs_nutritional_status, 
         childs_immunization_status, childs_diarrhoea_incidence, 
         access_to_healthcaare_services)




## Table 1

# Descriptive Analysis ----------------------------------------------------
#Creating a summary table of the entire dataset
Malnutrition_clean2 %>% 
  tbl_summary(by = quality_of_mothers_diet,
              statistic = 
                list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p})"),
              digits = list(all_continuous() ~ 2,
                            all_categorical() ~ c(0, 2)),
              type = list(where(is.character) ~ "categorical"),
              percent = "row",
              label = list(
                mothers_age ~ "Mother's age", 
                mothers_age_category ~ "Mother's age category", 
                mother_bmi ~ "Mother's BMI", 
                bmi_category ~ "Mother's BMI category",
                mothers_education ~ "Mother's education level", 
                #quality_of_mothers_diet ~ "Quality of mother's diet", 
                mothers_haemoglobin ~ "Mother's haemoglobin level",
                breastfeeding_status ~ "Breastfeeding status", 
                household_income ~ "Household income", 
                childs_gender ~ "Child's gender", 
                childs_age ~ "Child's age", 
                childs_height ~ "Child's height", 
                childs_weight ~ "Child's weight", 
                childs_nutritional_status ~ "Child's nutritional status", 
                childs_immunization_status ~ "Child's immunization status", 
                childs_diarrhoea_incidence ~ "Child's number of diarrhoea episode ", 
                access_to_healthcaare_services ~ "Access to healthcare services"), 
              missing_text = "(Missing)") %>% #stratify them by quality of mother’s diet
  add_overall() %>%  #adding overall column
  add_p() %>%  #adding p_value
  #modify_footnote( all_stat_cols() ~ "Mean (SD) or Frequency (%)") %>%
  modify_caption("**Table 1.  Frequency table of socio-demographic and nutrition indices of child by quality of mother's diet**") %>%
  bold_labels()




## Table 2
#Creating a table of mother’s bmi status by child’s nutritional status 
Malnutrition_clean2 %>% 
  select(childs_nutritional_status, mother_bmi, bmi_category) %>% 
  drop_na(childs_nutritional_status) %>% 
  tbl_summary(by = childs_nutritional_status,
              statistic = list(
                all_continuous() ~ "{mean} ({sd})",
                all_categorical() ~ "{n} ({p})"),
              digits = list(all_continuous() ~ 2,
                            all_categorical() ~ c(0, 2)),
              percent = "row",
              label = list(
                mother_bmi ~ "Mother's BMI category"), 
              missing_text = "(Missing)") %>%
  #modify_footnote( all_stat_cols() ~ "Mean (SD) or Frequency (%)") %>%
  modify_caption("**Table 2.  Mother's BMI by child's nutritional status**") %>%
  bold_labels()



## Figure 1
# Data Visualization ------------------------------------------------------

#a bar chart showing mother’s education 
Malnutrition_clean2 %>% 
  ggplot(mapping = aes(x = mothers_education,
                       fill = mothers_education))+
  geom_bar() +
  labs(x = "Mother's Education",
       y = "Frequency",
       title = "Mother's Education Level",
       fill = "Mother's Education")


## Figure 2
#A histogram showing child’s height
Malnutrition_clean2 %>% 
  drop_na(childs_height) %>% 
  ggplot(mapping = aes(x = childs_height))+
  geom_histogram(fill = "navyblue", colour = "black") +
  labs(x = "Child's height (cm)",
       y = "Frequency",
       title = "Distribution of height of children")

##Figure 3
#A scatter plot showing the relationship between mother’s bmi and child’s weight 
Malnutrition_clean2 %>% 
  ggplot(mapping = aes(x = mother_bmi,
                       y=childs_weight,
                       color = 'red'))+
  geom_point(size = 2)+
  geom_smooth(method = "lm",
              se = FALSE)+
  labs(x = "Mother's BMI (kg/sqm",
       y = "Child's weight (kg)" ,
       title = "Scatterplot showing relationship between mother's BMI and child's weight")


# Statistical Analysis ---------------------------------------------------

### 
#Hypothesis Testing
#1. Research Question: Is there a difference in the mean bmi of mothers whose diet are poor, average and good?

#2. Hypothesis: H0: There is no difference in the mean bmi of mothers whose diet are poor, average and good.
#H1: There is a difference in the mean bmi of mothers whose diet are poor, average and good
#3. Observation: 
Malnutrition_clean %>% 
  filter(`Quality of mother's diet` %in% c("Poor", "Average", "Good")) %>% 
  drop_na(mother_bmi) %>% 
  group_by(`Quality of mother's diet`) %>% 
  summarise(mean_bmi = mean(mother_bmi))

#Comment: there is an observed difference in mean bmi of mothers whose diets are poor, average and good.
#Is this observed difference statistically significant?

#Statistical significance testing
Malnutrition_clean %>% 
  filter(`Quality of mother's diet` %in% c("Poor", "Average", "Good")) %>% 
  aov(mother_bmi ~ `Quality of mother's diet`, #Using Anova because we have the mean of three variables to consider
      data = .) %>% 
  TukeyHSD()


#P_value is less than <0.05, hence there is enough evidence to accept the Null hypothesis

#In conclusion, there is a significant difference in the mean bmi of mothers whose diet are poor, average and good (P = 0.0381)



### 
#b.	Is the weight of children who were partially breastfed less than that of those who were exclusively breastfed?
#Hypothesis testing
#1. Research Question: Is the weight of children who were partially breastfed less than that of those who were exclusively breastfed?
#2. Hypothesis:H0: The weight of children who were partially breastfed is not less than that of those who were exclusively breastfed
#H1: the weight of children who were partially breastfed is less than that of those who were exclusively breastfed.

#3. Observation:
Malnutrition_clean %>%        #Observing for differences in mean
  filter(`Breastfeeding Status` %in% c("Partially", "Exclusively")) %>% 
  drop_na(`Child's weight`) %>% 
  group_by(`Breastfeeding Status`) %>% 
  summarise(mean_weight_of_children = mean(`Child's weight`))

#Comment: There is an observed difference in the mean weight of children who were Partially Breastfed and those Exclusively breastfed
#Is this difference statistically significance

#Statistical significance testing
#We will be using a One-sided t.test because we have a direction and two means to compare
Malnutrition_clean %>%  
  filter(`Breastfeeding Status` %in% c("Partially", "Exclusively")) %>% 
  t.test(`Child's weight` ~ `Breastfeeding Status`,
         data = .,
         alternative = "less",
         conf.level = 0.95)

#P_value is > 0.05 hence, there is enough evidence to accept the Null hypothesis
#In conclusion, there is no significance difference in the mean weight of Children who were partially breastfed and those exclusively breastfed


##c.	Is the proportion of children with up-to-date and not up-to-date immunization status different by children’s gender?
#1. Research Question: Is the proportion of children with up-to-date and not up-to-date immunization status different by children’s gender?
#2. Hypothesis: H0: the proportion of children with up-to-date and not up-to-date immunization status is not different by children’s gender?
#H1: the proportion of children with up-to-date and not up-to-date immunization status different by children’s gender?

#3. Observation: 
Malnutrition_clean %>% 
  filter(`Child's immunization status` %in% c("up-to-date", "not up-to-date")) %>% 
  drop_na(`Child's gender`) %>% 
  ggplot(mapping = aes(`Child's gender`, fill = `Child's immunization status`)) +
  geom_bar()
#Comment: There is an observe difference in the proportion of children with up-to-date and not up-to-date immunization status different by children’s gender

#4. Statistical significance testing: We will be using a Chi-square test of independence
Malnutrition_clean %>% 
  select(`Child's gender`, `Child's immunization status`) %>% 
  table() %>% 
  chisq.test()
#P-value > 0.05, there is enough evidence to accept the Null Hypothesis
#In conlusion, there is no significant difference in the proportion of children with up-to-date and not up-to-date immunization status by Gender.


