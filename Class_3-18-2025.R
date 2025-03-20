#Class 3-18-2025
#adding an interaction term to linear models 
library(tidyverse)
library(palmerpenguins)
library(GGally) # ggPairs()
library(ggiraph)
library(ggiraphExtra) # ggPredict()
library(broom)  # tidy() augment() #does NOT load with tidyverse
library(car) # vif()

################################################################################
penguins_lm_3 = penguins %>%
  filter(!is.na(bill_depth_mm),
         !is.na(bill_length_mm),
         !is.na(species))

lm_3 = lm(bill_depth_mm ~ bill_length_mm + species, data=penguins_lm_3)
lm_4 = lm(bill_depth_mm ~ bill_length_mm*species, data=penguins_lm_3)

AIC(lm_3, lm_4)

best_model = step(lm_4)

lm_4_predict = lm_4 %>%
  augment(se_fit=TRUE, interval="confidence") 

ggplot(data=lm_4_predict) +
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, x = bill_length_mm, fill = species, color = NULL), alpha = .15) +
  geom_line(aes(y = .fitted, x = bill_length_mm, color=species), size = 1) + 
  theme_bw()

################################################################################
#multiple regression with 2 or more continuous variables
gentoos = penguins %>%
  filter(species=="Gentoo")

lm_gentoo1= lm(data= gentoos, bill_depth_mm ~ bill_length_mm)

lm_gentoo2= lm(data= gentoos, bill_depth_mm ~ bill_length_mm + flipper_length_mm)

lm_gentoo3= lm(data= gentoos, bill_depth_mm ~ bill_length_mm + flipper_length_mm + body_mass_g)

AIC(lm_gentoo1, lm_gentoo2, lm_gentoo3)

best_model = step(lm_gentoo3)
summary(best_model)
vif(lm_gentoo3)

newdata = gentoos %>% 
  select(body_mass_g) %>% # or distinct(body_mass_g) to remove repeats
  mutate(bill_length_mm = median(gentoos$bill_length_mm, na.rm=TRUE), 
         flipper_length_mm = median(gentoos$flipper_length_mm, na.rm=TRUE))

lm_gentoo_3_predict = lm_gentoo3 %>%
  broom::augment(newdata = newdata, se_fit=TRUE, interval="confidence") 

ggplot(data=lm_gentoo_3_predict) +
  geom_point(aes(x = body_mass_g, y = bill_depth_mm), data=gentoos) + # original data
  geom_ribbon(aes(ymin = .lower, ymax = .upper, x = body_mass_g), alpha = .15) +
  geom_line(aes(y = .fitted, x = body_mass_g)) +
  annotate("text", x=4250, y=17, label= paste0("flipper length = ", median(gentoos$flipper_length_mm, na.rm=TRUE), "mm")) +
  annotate("text", x=4250, y=16.5, label= paste0("bill length = ", median(gentoos$bill_length_mm, na.rm=TRUE), "mm")) 

################################################################################
#ANOVA
penguin_lm = lm(body_mass_g ~ species + sex, data=penguins)
summary(penguin_lm)

anova(penguin_lm)

# Conduct the same ANOVA using aov()
penguin_anova = aov(body_mass_g ~ species + sex, data=penguins)
summary(penguin_anova)

# which sex has higher body mass?
penguins %>%
  group_by(sex) %>%
  summarize(mean_body_mass_g = mean(body_mass_g))

# which species has higher body mass?
penguins %>%
  group_by(species) %>%
  summarize(mean_body_mass_g = mean(body_mass_g, na.rm=TRUE))

TukeyHSD(penguin_anova)  # Requires the output of the aov() function



