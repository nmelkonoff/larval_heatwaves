#Natalie Melkonoff
#ANOVA for larval heatwaves
#Feb 8 2024

library(car)
library(ggplot2)
library(tidyverse)

#is fwl dependent on hostplant spp and/or treatment?
model <- aov(r2_adult$fwl ~ r2_adult$hostplant * r2_adult$treatment, 
             data = r2_adult)

summary(model)

#tests for normality
plot(model, which = 2)

qqPlot(model$residuals, id = FALSE)

#boxplot
data <- r2_adult
ggplot(data) +
  aes(x = data$hostplant, y = data$fwl, fill = data$treatment) +
  geom_boxplot()

data %>%
  filter(!is.na(data$treatment)) %>%
  ggplot() +
  aes(x = data$hostplant, y = data$fwl, fill = data$treatment) +
  geom_boxplot()

ggplot(data) +
  aes(x = data$treatment, y = data$fwl, fill = data$hostplant) +
  geom_boxplot()

#is larval days dependent on hostplant spp and/or treatment?
model2 <- aov(r2_adult$larval_days ~ r2_adult$hostplant * r2_adult$treatment, 
             data = r2_adult)

summary(model2)

ggplot(data) +
  aes(x = data$hostplant, y = data$larval_days, fill = data$treatment) +
  geom_boxplot()

#is pupal days dependent on hostplant spp and/or treatment?
model3 <- aov(r2_adult$pupal_days ~ r2_adult$hostplant * r2_adult$treatment, 
              data = r2_adult)
summary(model3)
##no significant relationships
