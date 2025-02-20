#Class 2-20-2025
library(tidyverse)
library(palmerpenguins)

find("filter")

gentoo= penguins %>%
  dplyr::filter(species== "Gentoo") %>% #this tells which package to use for the function
  dplyr::select(-body_mass_g) %>%
  print()

penguins_no_nas= penguins %>%
  filter(!is.na(sex))

#scatter plot
ggplot(data=penguins_no_nas) +
  geom_point(aes(x= flipper_length_mm, y= body_mass_g, color=species, shape=sex)) + 
  geom_smooth(aes(x= flipper_length_mm, y= body_mass_g)) +
  xlab("Flipper Length (mm)") +
  ylab("Body Mass (g)") +
  ggtitle("Penguins are cute :)") +
  theme_bw()

#line plot
penguins_ts= penguins %>%
  group_by(species, year) %>%
  summarize(count=n())
penguins_ts

ggplot(data= penguins_ts) +
  geom_line(aes(x=year, y=count, color=species)) +
  ylab("Number of Penguins Sampled") +
  xlab("Year") +
  theme_classic()

#histogram 
ggplot(data=penguins) + 
  geom_histogram(aes(x=flipper_length_mm, fill=species), 
                 binwidth = 2, #makes the "bins" 2mm
                 position = "identity", #layers the histogram by species 
                 alpha= 0.7) +#makes the colors transparent
  scale_fill_manual(values=c("darkorange", "darkorchid", "cyan4"))

#box plots
ggplot(data=penguins) +
  geom_boxplot(aes(y= flipper_length_mm, x=species))+
  geom_jitter(aes(y=flipper_length_mm, x=species, color=species),
              width=0.2) #randomly move data points side to side within species

#bar charts
ggplot(data=penguins) +
  geom_bar(aes(x=island, fill=species))+
  facet_wrap(~species, nrow=3) + #make different panel for each category in this column
  coord_flip() #flips the axis 

#save your plot
ggsave("figures/penguins_islands_species.pdf", device="pdf", 
       units= "in", width=5, height= 7, dpi=300)

#Exercise 2.2
ggplot(data=penguins_no_nas)+
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm, color=sex))+
  facet_wrap(~species)+
  xlab("Bill Length (mm)")+
  ylab("Bill Depth (mm)")
  








