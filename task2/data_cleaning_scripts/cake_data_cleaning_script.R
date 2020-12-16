#This is an R script for the cleansing of the cake data - Task 2

#Load Tidyverse
library(tidyverse)

#load in Janitor
library(janitor)

#Read in the data
cake_data <- read_csv("raw_data/cake-ingredients-1961.csv")
cake_codes <- read_csv("raw_data/cake_ingredient_code.csv")

#----------------------------------------------
#check data dimension
dim(cake_data)

#check the variable names
names(cake_data)
#view the data
view(cake_data)

#--------------------------------------------------

cake_data_longer <- cake_data %>% 
  pivot_longer(cols = c(AE:ZH),
               names_to = "ingredients",
               values_to = "amount")
               
view(cake_data_longer)

cake_data_simplified <- cake_data_longer %>% 
  drop_na()

clean_cake_data_simplified <- clean_names(cake_data_simplified)
view(clean_cake_data_simplified)

view(clean_cake_data_simplified)

view(cake_codes)

cake_date_joined <- inner_join(clean_cake_data_simplified, cake_codes,
           by=c("ingredients" = "code"))

view(cake_date_joined)

#cake_date_joined$ingredients <- NULL

nulled_cakes <- cake_date_joined %>% 
  select(-ingredients)

view(nulled_cakes)

new_order <- c("cake", "ingredient", "amount", "measure")


rearranged_cakes <- nulled_cakes[, new_order]

view(rearranged_cakes)

#----------------------------------------
#rearranged_cakes is the final version of the data this will
#be written to another file and used in the analysis part

#1.Which cake has the most cocoa in it?
rearranged_cakes %>% 
  filter(ingredient == "Cocoa") %>%
  slice_max(amount, n = 1)

#2. For sponge cake, how many cups of ingredients are used in total?

rearranged_cakes %>%
  filter(measure == "cup" & cake == "Sponge") %>% 
  summarise(total = sum(amount))

#3. How many ingredients are measured in teaspoons?

rearranged_cakes %>% 
  filter(measure == "teaspoon") %>% 
  distinct(ingredient) %>% 
  summarise(count = n()) 

 #4. Which cake has the most unique ingredients?
cake_data_ingredient_counted <- rearranged_cakes %>% 
  group_by(ingredient) %>% 
    mutate(ingredient_count = n()) #%>% 
    #arrange((ingredient_count))

#view (cake_data_ingredient_counted)

cake_data_ingredient_counted %>%
  group_by(cake) %>% 
  summarise(
    total = sum(ingredient_count)
) %>% 
  slice_min(total)
  #arrange((total)) 


#5. Which ingredients are used only once?

rearranged_cakes %>%
  group_by(ingredient) %>% 
  summarise(count = n()) %>%
  filter(count == 1)
  #slice_min(count)
  
