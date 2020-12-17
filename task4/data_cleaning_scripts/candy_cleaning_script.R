
library(tidyverse)
library(janitor)
library(readxl)
library(openxlsx)

candy_data_2015 <- read_excel("raw_data/boing-boing-candy-2015.xlsx")
candy_data_2016 <- read_excel("raw_data/boing-boing-candy-2016.xlsx")
candy_data_2017 <- read_excel("raw_data/boing-boing-candy-2017.xlsx")


#rename columns needed for data analysis in 2015 data
candy_data_2015_1 <- candy_data_2015 %>%
  rename("age" = "How old are you?") %>% 
  rename("trick_or_treat" = 
           "Are you going actually going trick or treating yourself?") %>%
  #added a column for country, year & gender
  mutate(country = NA) %>% 
  mutate(year = "2015") %>% 
  mutate(gender = NA)

#select the appropriate columns for '15
candy_data_2015_2 <- candy_data_2015_1 %>%
  select(age, trick_or_treat, country, year, gender, starts_with("["))

#make the data tidy - setting all variables as columns (sweets as a column)
candy_data_2015_longer <- candy_data_2015_2 %>% 
  pivot_longer(cols = c(starts_with("[")),
               names_to = "sweets",
               values_to = "opinion")


#rename columns needed for data analysis in 2016 data
candy_data_2016_1 <- candy_data_2016 %>%
  rename("age" = "How old are you?") %>% 
  rename("trick_or_treat" = 
           "Are you going actually going trick or treating yourself?") %>% 
  rename("gender" = "Your gender:") %>% 
  rename("country" = "Which country do you live in?") %>% 
  mutate(year = "2016")
  
#select the appropriate columns from 2016 data
candy_data_2016_2 <- candy_data_2016_1 %>%
  select(age, trick_or_treat, country, year, gender, starts_with("["))


#make the data tidy - setting all variables as columns (sweets as a column)
candy_data_2016_longer <- candy_data_2016_2 %>% 
  pivot_longer(cols = c(starts_with("[")),
               names_to = "sweets",
               values_to = "opinion")

#combine the data for '15 & '16
candy_data_combined_15_16 <- bind_rows(candy_data_2015_longer, 
                                   candy_data_2016_longer) 

#remove the '[]' around the sweet names for 15-16 data
final_data_15_16 <- candy_data_combined_15_16 %>%
  mutate(sweets = str_remove_all(sweets, "\\[")) %>% 
  mutate(sweets = str_remove_all(sweets, "\\]"))
  

#rename columns needed for data analysis in 2017 data
candy_data_2017_1 <- candy_data_2017 %>%
  rename("age" = "Q3: AGE") %>%
  rename("trick_or_treat" = 
           "Q1: GOING OUT?") %>%
  rename("gender" = "Q2: GENDER") %>%
  rename("country" = "Q4: COUNTRY") %>%
  #add a column for year
  mutate(year = "2017")

#select the appropriate columns from 2017 data
candy_data_2017_2 <- candy_data_2017_1 %>%
  select(age, trick_or_treat, country, year, gender, starts_with("Q6"))

#make the data tidy - setting all variables as columns (sweets as a column)
candy_data_2017_longer <- candy_data_2017_2 %>% 
  pivot_longer(cols = c(starts_with("Q6")),
               names_to = "sweets",
               values_to = "opinion")

#remove the "Q6" from the sweet names
final_data_17 <- candy_data_2017_longer %>% 
  mutate(sweets = str_remove_all(sweets, "Q6 \\| "))

#combine the data from all 3 years
candy_data_combined_15_16_17 <- bind_rows(final_data_15_16, 
                                      final_data_17)


#extract only numeric characters from age column
candy_data_combined_15_16_17 <- candy_data_combined_15_16_17 %>%
  mutate(age = str_extract(age, "[0-9]?[0-9]?[0-9]?")) %>% 
  mutate(age = as.numeric(age)) %>% 
  #only include ages between 0-99 (I assume babies get 
  #counted whilst parents eat candy on their behalf!)
 mutate(age = ifelse(age > 99, NA, age))

#remove ambiguously specified gender category
candy_data_combined_15_16_17 <- candy_data_combined_15_16_17 %>% 
  mutate(gender = ifelse(gender =="I'd rather not say", NA, gender))

   
#add columns to convert opinions to numeric values for Qs 5,6,7
candy_data_combined_15_16_17 <- candy_data_combined_15_16_17 %>%
  mutate(opinion_int = ifelse(opinion == "JOY", 1,
  ifelse(opinion == "DESPAIR", -1, 
  ifelse(opinion == "MEH", 0, opinion)))) %>% 
  mutate(opinion_int = as.numeric(opinion_int))


#create new country column to contain a subset of nations - only America, UK, Canada & other
candy_data_final <- candy_data_combined_15_16_17 %>%
  mutate(country_subset = ifelse(str_detect(country, "[Aa][Mm][Ee][Rr][Ii][Cc][Aa]"), "america",
                ifelse(str_detect(country, "[Uu][Ss][Aa]"), "america",
                ifelse(str_detect(country, "[Ss][Tt][Aa][Tt][Ee]"), "america",
                ifelse(str_detect(country, "[Uu][Nn][Ii][Tt][Ee][Dd] [Ss]"), "america", 
                ifelse(str_detect(country, "[Uu][Ss]"), "america",
                ifelse(str_detect(country, "[Uu][:punct:][Ss][:punct:][Aa]*"), "america",
                ifelse(str_detect(country, "[Uu][:punct:][Ss][:punct:]"), "america",
                ifelse(str_detect(country, "[Uu] [Ss] "), "america",
                ifelse(str_detect(country, "merica"), "america",
                ifelse(str_detect(country, "[Uu] [Ss]"), "america",
                ifelse(str_detect(country, "Yoo Ess"), "america",
                ifelse(str_detect(country, "Trump"), "america",
                ifelse(str_detect(country, "[:punct:][Aa][Mm][Ee][Rr][Ii][Cc][Aa]"), "america",
                ifelse(str_detect(country, "[Cc][Aa][Nn]"), "canada",
                ifelse(str_detect(country, "[Uu][Nn][Ii][Tt][Ee][Dd] [Kk]"), "UK",
                ifelse(str_detect(country, "[Uu][:punct:][Kk]"), "UK",
                ifelse(str_detect(country, "[Ee][Nn][Gg][Ll][Aa][Nn][Dd]"), "UK",
                ifelse(str_detect(country, "[Ss][Cc][Oo][Tt][Ll][Aa][Nn][Dd]"), "UK", "other"
                                 )))))))))))))))))))
                          
#final version of data saved to separate folder
write.xlsx(candy_data_final, "clean_data/candy_data_final3.xlsx")



#========================================
#========================================

#1. What is the total number of candy ratings given across the three years. 
#(number of candy ratings, not number of raters. Don’t count missing values)

#returns the number of opinions given which are not NA
candy_data_final %>%
    filter(!is.na(opinion)) %>% 
    summarise(count = n())

#2. What was the average age of people who are going out trick or treating 
#and the average age of people not going trick or treating?

#answer when going out trick or treating
candy_data_final %>%
  filter(trick_or_treat == "Yes") %>% 
  summarise(average_age = median(age, na.rm = TRUE))


#answer when not going out trick or treating
candy_data_final %>%
  filter(trick_or_treat == "No") %>% 
  summarise(average_age = median(age, na.rm = TRUE))

  
#3. For each of joy, despair and meh, 
#which candy bar revived the most of these ratings?

candy_data_final %>% 
    filter(grepl("[Bb][Aa][Rr]", sweets)) %>% 
    filter(!is.na(opinion)) %>% 
    group_by(opinion, sweets) %>% 
    summarise(count = n()) %>% 
    filter(count == max(count)) %>% 
    arrange(desc(count))
   
  
  #4. How many people rated Starburst as despair?
  
candy_data_final %>%
    filter(grepl("[Ss][Tt][Aa][Rr][Bb][Uu][Rr][Ss][Tt]", sweets)) %>%
    filter(opinion == "DESPAIR") %>%
    summarise(count = n())
  
  
  #
  #Where is question5????????
  #
  
  #6.What was the most popular candy bar by this 
  #rating system for each gender in the dataset?
  

candy_data_final %>%
    filter(!is.na(gender)) %>% 
    filter(grepl("[Bb][Aa][Rr]", sweets)) %>%
    filter(!is.na(opinion_int)) %>%
    group_by(gender, sweets) %>% 
    summarise(popularity = sum(opinion_int)) %>% 
    filter(popularity == max(popularity)) %>% 
    arrange(desc(popularity))
    
  
  #7. What was the most popular candy bar in each year?
candy_data_final %>%
    filter(grepl("[Bb][Aa][Rr]", sweets)) %>%
    filter(!is.na(opinion_int)) %>%
    group_by(year, sweets) %>% 
    summarise(popularity = sum(opinion_int)) %>% 
    filter(popularity == max(popularity)) %>% 
    arrange(desc(year))
  
  
  #8. What was the most popular candy bar by this rating for people in 
  #US, Canada, UK and all other countries?
  
candy_data_final %>%
    filter(!is.na(country_subset)) %>% 
    filter(grepl("[Bb][Aa][Rr]", sweets)) %>%
    filter(!is.na(opinion_int)) %>%
    group_by(country_subset, sweets) %>% 
    summarise(popularity = sum(opinion_int)) %>% 
    filter(popularity == max(popularity)) %>% 
    arrange(desc(popularity))
  





