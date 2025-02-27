#Class 2-27-2025
#3.5 Linear Models 

#Linear regression
#assumes linear relationship, normality of residuals, independent samples, variance is similar in the variables

library(tidyverse)
library(palmerpenguins)
library(GGally)

penguins %>%
  select(bill_length_mm, bill_depth_mm) %>%
  GGally::ggpairs()

lm_1= lm(bill_depth_mm ~ bill_length_mm, data = penguins) #build a linear model 
class(lm_1)
summary(lm_1)
lm_1$coefficients
lm_1$residuals

#plot the residuals
ggplot(data=penguins, aes(x=bill_length_mm, y=bill_depth_mm))+
  geom_point()+
  geom_smooth(method="lm")

plot(lm_1)

#same thing with just one species 
gentoo= penguins %>%
  filter(species=="Gentoo")

gentoo %>%
  select(bill_length_mm, bill_depth_mm) %>%
  ggpairs()
lm_2= lm(bill_depth_mm ~ bill_length_mm, data= gentoo)
summary(lm_2)

ggplot(data=gentoo, aes(x=bill_length_mm, y=bill_depth_mm))+
  geom_point()+
  geom_smooth(method= "lm")

#plot 4 models in the same plot 
ggplot(data=penguins)+
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm, color=species))+
  geom_smooth(aes(x=bill_length_mm, y=bill_depth_mm), method= "lm", color="black")+ #bad model that didnt account for species
  geom_smooth(aes(x=bill_length_mm, y=bill_depth_mm, color=species), method="lm") #model that creates a separate lines for the species

#Exercise 5.1
gentoo_lm_data= penguins %>%
  filter(species=="Gentoo") %>%
  select(bill_depth_mm, flipper_length_mm)

lm_3= lm(bill_depth_mm ~ flipper_length_mm, data=gentoo_lm_data)
summary(lm_3)

ggplot(data=gentoo_lm_data)+
  geom_point(aes(x=bill_depth_mm, y=flipper_length_mm))+
  geom_smooth(aes(x=bill_depth_mm, y=flipper_length_mm), method= "lm")







