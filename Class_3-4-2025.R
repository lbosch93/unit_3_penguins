#Class 3-4-2025
library(palmerpenguins)
library(tidyverse)
library(GGally)

#Multiple linear regression
penguins_lm3 = penguins %>%
  filter(!is.na(bill_depth_mm),
         !is.na(bill_length_mm),
         !is.na(species))

lm_3= lm(bill_depth_mm ~ bill_length_mm + species, data= penguins_lm3)
summary(lm_3)
coef(lm_3)
anova(lm_3)

popper= broom::tidy(lm_3, conf.int=TRUE)
popper

library(ggiraph)
library(ggiraphExtra)
ggPredict(lm_3, se=TRUE, interactive=TRUE) 
#the slopes are the same for each of the species 

##### model predictions in base R ##############################################
lm_3_predictions = predict(lm_3, interval= "confidence", level= 0.95) 
head(lm_3_predictions)
#original data is used to predict in the fit column, lower prediction
#and upper prediction use the confidence interval for limits 

#this is the original data and our model/predictions together!
penguins_lm_3_predict = cbind(penguins_lm3, lm_3_predictions)
head(penguins_lm_3_predict)

#lets plot the data and prediction model together
ggplot(data=penguins_lm_3_predict, 
       aes(x=bill_length_mm, y= bill_depth_mm,color= species)) +
  geom_point()+
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill=species, color= NULL), 
              alpha=0.3) + #this makes the polygon around the line
  geom_line(aes(y=fit), linewidth=1) + #this makes the y the predicted value rather than the data
  theme_bw()

#lets try to extrapolate with our model 
#look at the min and max values for the dataset that we have to create a new dataset
minbill= min(penguins$bill_length_mm, na.rm=TRUE)
maxbill=max(penguins$bill_length_mm, na.rm=TRUE)

#new data set using the min and max and stepping by 0.1
newdata_bill_length_mm= seq(from= minbill, to= maxbill, by= 0.1)
#i want the column to be the same name as the original data, and i want to have 
#a value for each theoretical bill length for each species 
new_data= expand.grid(bill_length_mm= newdata_bill_length_mm,
                     species= unique(penguins$species))
summary(newdata)

new_predictions= predict(lm_3, interval= "confidence", newdata = new_data)
head(new_predictions)

newdata_predict_lm3= cbind(new_data, new_predictions)
head(newdata_predict_lm3)

ggplot()+
  geom_point(data=penguins_lm3, 
             aes(x=bill_length_mm, y=bill_depth_mm, color= species)) +
  geom_ribbon(data=newdata_predict_lm3, 
              aes(x=bill_length_mm, ymin = lwr, ymax = upr, fill=species, color=NULL),
              alpha= 0.3)+
  geom_line(data = newdata_predict_lm3,
            aes(y=fit, x=bill_length_mm, color=species))

#### model predictions in tidyverse ############################################
lm_3_predict= lm_3 %>%
  broom::augment(data=penguins_lm3, se_fit= TRUE, interval= "confidence")
glimpse(lm_3_predict)

ggplot(data= lm_3_predict, 
       aes(x=bill_length_mm, y=bill_depth_mm, color=species))+
  geom_point()+
  geom_ribbon(aes(ymin = .lower, ymax= .upper, fill=species, color=NULL),
              alpha=0.5)+
  geom_line(aes(y=.fitted))+
  theme_bw()

newdata_again= penguins_lm3 %>%
  tidyr::expand(bill_length_mm, species)

lm_3_predict = lm_3 %>%
  broom::augment(newdata= new_data, se_fit=TRUE, interval= "confidence")





