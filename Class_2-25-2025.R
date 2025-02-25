#Class 2-25-2025
library(palmerpenguins)
library(rstatix)
library(knitr)
library(tidyverse)

#3.3 T-tests

#one sample t-test
#the data should be normally distributed and not have outliers (n=30)
#is the gentoo body mass different from a value found in the literature?
head(penguins)

gentoo= penguins %>%
  filter(species== "Gentoo")%>%
  filter(!is.na(bill_depth_mm))%>%
  filter(!is.na(bill_length_mm))

ggplot() +
  geom_histogram(aes(x=body_mass_g), data = gentoo)

#QQ plot tells us if there is a specific distribution to your data 
ggplot()+
  stat_qq(aes(sample=body_mass_g), data=gentoo)
  #the plot is a straight line, so it is normal and we can use the t-test

lit_body_mass_g= 5500
my_t_test=t.test(gentoo$body_mass_g, mu= lit_body_mass_g)
summary(my_t_test)
class(my_t_test)
my_t_test$statistic

gentoo_t_test= gentoo %>%
                t_test(body_mass_g ~ 1, mu= lit_body_mass_g)
gentoo_t_test$p

#################################################################################

#independent sample t-test
#assumes that the observations are independent, there are no outliers, 
#and that the data has a normal distribution

#compare the gentoo and the adelie body mass
data_for_t_test= penguins %>%
  filter(species %in% c("Gentoo", "Adelie")) %>% #we only want two species
  filter(!is.na(body_mass_g)) %>% #we want to remove those with NA
  select(species, body_mass_g) %>%#only include these two columns
  droplevels() #forget the chinstrap data because the value is 0

summary(data_for_t_test)

data_for_t_test %>% #lets create a tibble with the mean and sd for each species
  group_by(species) %>%
  summarize(avg=mean(body_mass_g), sd=sd(body_mass_g))

#lets check the distribution of the data sets 
ggplot() +
  geom_histogram(aes(x=body_mass_g, fill= species), data= data_for_t_test) +
  facet_wrap(~species)

ggplot() +
  stat_qq(aes(sample=body_mass_g, color=species), data= data_for_t_test) +
  facet_wrap(~species, scales= "free")

#lets check if the variances are similar in the two data sets
#null hypothesis is that the variances are similar 
data_for_t_test %>%
  levene_test(body_mass_g~species)

#we can move forward with the t test!!!
independent_t_test =t.test(data_for_t_test$body_mass_g ~ data_for_t_test$species,
                           var.equal=TRUE)
summary(independent_t_test)
independent_t_test$p.value
#OR
data_for_t_test %>%
  t_test(body_mass_g ~ species)

################################################################################
#Exercise 3.1

################################################################################
#3.4 Correlations 

#correlations bill lengths vs. bill length
#check data first
ggplot()+
  geom_point(data=gentoo, aes(x= bill_length_mm, y=bill_depth_mm))

ggplot()+
  stat_qq(data= gentoo, aes(sample=bill_length_mm))
ggplot()+
  stat_qq(data= gentoo, aes(sample=bill_depth_mm))

correlation_test= cor(x=gentoo$bill_depth_mm, y=gentoo$bill_length_mm, use= "complete.obs")
#OR
hand_test= cor.test(x=gentoo$bill_depth_mm, y=gentoo$bill_length_mm, use="complete.obs")
#OR
rstatix_t_test= gentoo %>%
  cor_test(bill_length_mm, bill_depth_mm)

cor(gentoo[,3:6], use="complete.obs") #correlation of the columns 3, 4, 5, and 6

#this is an extension of ggplot
install.packages("GGally")
library("GGally")

gentoo %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  ggpairs()

penguins %>%
  select(ends_with("_mm"), body_mass_g, species) %>%
  ggpairs(aes(color=species))












