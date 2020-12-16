
library(tidyverse)
library(janitor)
library(readxl)

candy_data_2015 <- read_excel("raw_data/boing-boing-candy-2015.xlsx")
candy_data_2016 <- read_excel("raw_data/boing-boing-candy-2016.xlsx")
candy_data_2017 <- read_excel("raw_data/boing-boing-candy-2017.xlsx")


view(candy_data_2015)
view(candy_data_2016)
view(candy_data_2017)

#rename columns needed for data analysis in 2015 data
candy_data_2015_1 <- candy_data_2015 %>%
  rename("age" = "How old are you?") %>% 
  rename("trick_or_treat" = 
           "Are you going actually going trick or treating yourself?") %>%
  #added a column for country, year & gender
  mutate(country = "Unknown") %>% 
  mutate(year = "2015") %>% 
  mutate(gender = "Unknown")

#view(candy_data_2015_1)

candy_test <- candy_data_2015_1 %>%
  select(age, trick_or_treat, country, year, gender, starts_with("["))

candy_test_longer <- candy_test %>% 
  pivot_longer(cols = c(starts_with("[")),
               names_to = "sweets",
               values_to = "opinion")

#view(candy_test_longer)


#rename columns needed for data analysis in 2016 data
candy_data_2016_1 <- candy_data_2016 %>%
  rename("age" = "How old are you?") %>% 
  rename("trick_or_treat" = 
           "Are you going actually going trick or treating yourself?") %>% 
  rename("gender" = "Your gender:") %>% 
  rename("country" = "Which country do you live in?") %>% 
  mutate(year = "2016")
  
#select the approraite columns from 2016 data
candy_data_2016_test <- candy_data_2016_1 %>%
  select(age, trick_or_treat, country, year, gender, starts_with("["))

#view(candy_data_2016_test)

#make the data tidy - setting all variables as columns (sweets as a column)
candy_data_2016_test_longer <- candy_data_2016_test %>% 
  pivot_longer(cols = c(starts_with("[")),
               names_to = "sweets",
               values_to = "opinion")

#view(candy_data_2016_test_longer)

candy_data_combined_15_16 <- rbind(candy_test_longer, 
                                   candy_data_2016_test_longer) 

#remove the '[]' around the sweet names for 15-16 data
final_data_15_16 <- candy_data_combined_15_16 %>%
  mutate(sweets = str_remove_all(sweets, "\\[")) %>% 
  mutate(sweets = str_remove_all(sweets, "\\]"))
  
#view(final_data_15_16)


#view(candy_data_combined_15_16)
#write_excel_csv(candy_data_combined_15_16, "raw_data/excel_test2.csv")


#rename 2017 columns
candy_data_2017_1 <- candy_data_2017 %>%
  rename("age" = "Q3: AGE") %>%
  rename("trick_or_treat" = 
           "Q1: GOING OUT?") %>%
  rename("gender" = "Q2: GENDER") %>%
  rename("country" = "Q4: COUNTRY") %>%
  #add a column for year
  mutate(year = "2017")

#select the appropriate columns from 2016 data
candy_data_2017_test <- candy_data_2017_1 %>%
  select(age, trick_or_treat, country, year, gender, starts_with("Q6"))

#make the data tidy - setting all variables as columns (sweets as a column)
candy_data_2017_test_longer <- candy_data_2017_test %>% 
  pivot_longer(cols = c(starts_with("Q6")),
               names_to = "sweets",
               values_to = "opinion")

#remove the "Q6" from the sweet names
final_data_17 <- candy_data_2017_test_longer %>% 
  mutate(sweets = str_remove_all(sweets, "Q6 \\|"))

#combine the data from the 3 years
candy_data_combined_15_16_17 <- rbind(final_data_15_16, 
                                      final_data_17)





#df %>% filter(!grepl("^1", y))



#========================================
#========================================

#1. What is the total number of candy ratings given across the three years. 
#(number of candy ratings, not number of raters. Don’t count missing values)

#returns the number of opinions given which are not NA
candy_data_combined_15_16_17 %>%
    filter(!is.na(opinion)) %>% 
    summarise(count = n())

#2. What was the average age of people who are going out trick or treating 
#and the average age of people not going trick or treating?





#write_excel_csv(filtered, "raw_data/filtered2.csv")

#filtered2 <- candy_data_combined_15_16_17 %>%
 # filter(grepl("^[a-z]+", age))

#view(filtered2)
#write_excel_csv(filtered2, "raw_data/grepl.csv")



#=======================================================
#write_excel_csv(candy_data_combined_15_16_17, 
   #             "raw_data/excel_data_combined.csv")

#=======================================================



#more clean up on combined data




#-----------------------------------------
#Graham A code test
#filtered <- candy_data_2016_1 %>% 
 # filter(is.na(age))

#view(filtered)
#-------------------------------------------