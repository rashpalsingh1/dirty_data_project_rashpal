#This is an R script for the cleansing of the cake data - Task 2

#Load Tidyverse
library(tidyverse)

#load in Janitor
library(janitor)

#Read in the data
cake_data <- read_csv("raw_data/cake-ingredients-1961.csv")
cake_codes <- read_csv("raw_data/cake_ingredient_code.csv")


#make the data tidy, setting all variables as columns
cake_data_longer <- cake_data %>% 
  pivot_longer(cols = c(AE:ZH),
               names_to = "ingredients",
               values_to = "amount")
               
#remove NA values
cake_data_simplified <- cake_data_longer %>% 
  drop_na()

#create standardized column heading using clean_names
clean_cake_data_simplified <- clean_names(cake_data_simplified)

#join the two tables to obtain the full names of ingredients
cake_date_joined <- inner_join(clean_cake_data_simplified, cake_codes,
           by=c("ingredients" = "code"))

#drop the codes column as it is no longer required
final_cakes <- cake_date_joined %>% 
  select(-ingredients)


write_csv(final_cakes, "clean_data/final_cakes.csv")

#----------------------------------------
#final_cakes is the final version of the data this will
#be written to another file and used in the analysis part

#1.Which cake has the most cocoa in it?
final_cakes %>% 
  filter(ingredient == "Cocoa") %>%
  slice_max(amount, n = 1)

#2. For sponge cake, how many cups of ingredients are used in total?

final_cakes %>%
  filter(measure == "cup" & cake == "Sponge") %>% 
  summarise(total = sum(amount))

#3. How many ingredients are measured in teaspoons?

final_cakes %>% 
  filter(measure == "teaspoon") %>% 
  #need to count each ingredient once only
  distinct(ingredient) %>% 
  summarise(count = n()) 


 #4. Which cake has the most unique ingredients?
ingredient_count <- final_cakes %>% 
  group_by(cake) %>% 
    mutate(total = sum(ingredient_count = n())) 
ingredient_count %>% 
  summarise(cake_total = sum(total)) %>% 
  arrange(cake_total)


#5. Which ingredients are used only once?

final_cakes %>%
  group_by(ingredient) %>% 
  summarise(count = n()) %>%
  filter(count == 1)




