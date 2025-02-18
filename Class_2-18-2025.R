#Class 2-18-2025
install.packages("tidyverse")
library("tidyverse")
tidyverse_packages()

#bring in penguin data
install.packages("palmerpenguins")
library("palmerpenguins")

head(penguins)
tail(penguins)
summary(penguins)
glimpse(penguins)
dim(penguins)

#dplyr
gentoo = filter(penguins, species== "Gentoo")
summary(gentoo)

gentoo_ladies= filter(penguins, species== "Gentoo", sex== "female")
summary(gentoo_ladies)

#this takes multiple commands and strings them together, kinda like the + sign
# %>%

gentoo= penguins %>% 
  filter(species== "Gentoo") %>%
  filter(sex== "female")


female_mass= penguins %>%
  filter(sex== "female") %>%
  summarize(mean_mass_g = mean(body_mass_g))
female_mass

species_mean_mass_g = penguins %>%
  filter(!is.na(sex)) %>% #get rid of the values that weren't sexed 
  group_by(species, sex, island) %>%
  summarize(mean_mass_g = mean(body_mass_g, na.rm= TRUE), count= n()) %>%
  print()
write.csv(file="data/processed/mass_table.csv", x=species_mean_mass_g) 

penguins_for_america= penguins %>%
  mutate(body_mass_lb= body_mass_g * 0.0022) #conversion: 0.0022 lbs/g
penguins_for_america


islands= penguins %>% 
  distinct(island)

penguins_brief = penguins %>%
  select(-body_mass_g) %>%
  print()

penguins_sorted= penguins %>%
  arrange(desc(body_mass_g), desc(bill_depth_mm)) %>%
  print()

#Exercise 1.3
penguin_bill_inches= penguins %>%
  filter(species== "Adelie", island!= "Torgersen") %>%
  mutate(bill_length_in= bill_length_mm / 25.4) %>% #one inch = 25.4 mm 
  summarize(mean_bill_length_in= mean(bill_length_in), 
            std_bill_length_in= sd(bill_length_in)) %>%
  print()






