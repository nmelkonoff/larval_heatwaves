#Natalie Melkonoff
#ANOVA for larval heatwaves
#Feb 8 2024

library(car)
library(ggplot2)
library(tidyverse)
library(dplyr)

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

#calculate meas and sds

#Function to calculate the mean and the standard deviation for each group

#data: a data frame
#varname: the name of a column containing the variable to be summarized
#groupnames: vector of column names to be used as grouping variables

#fwl
data_summary_fwl <- function(data, varname = "fwl", groupnames = c("hostplant", "treatment")){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}


df_fwl <- data_summary_fwl(data, varname ="fwl", 
                              groupnames = c("hostplant", "treatment"))

df_fwl$hostplant=as.factor(df_fwl$hostplant)
head(df_fwl)

#plot fwl x hostplant x treatment

p <- ggplot(df_fwl, aes(x=hostplant, y=fwl, fill=treatment)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=fwl-sd, ymax=fwl+sd), width=.2,
                position=position_dodge(.9))

p + scale_fill_manual(values = c("darkblue", "darkgreen", "purple", "red", "yellow"))

#pupal_days
data_summary_pupal_days <- function(data, varname = "pupal_days", groupnames = c("hostplant", "treatment")){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}


df_pupal_days <- data_summary_pupal_days(data, varname ="pupal_days", 
                       groupnames = c("hostplant", "treatment"))

#plot pupal days x hostplant x treatment

p_pupal_days <- ggplot(df_pupal_days, aes(x=hostplant, y=pupal_days, fill=treatment)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=pupal_days-sd, ymax=pupal_days+sd), width=.2,
                position=position_dodge(.9))

p_pupal_days + scale_fill_manual(values = c("darkblue", "darkgreen", "red", "yellow"))


#pupal_mass
data_summary_pupal_mass <- function(data, varname = "pupal_mass", groupnames = c("hostplant", "treatment")){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}


df_pupal_mass <- data_summary(data, varname ="pupal_mass", 
                       groupnames = c("hostplant", "treatment"))

#plot pupal mass x hostplant x treatment
p_pupal_mass <- ggplot(df_pupal_mass, aes(x=hostplant, y=pupal_mass, fill=treatment)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=pupal_mass-sd, ymax=pupal_mass+sd), width=.2,
                position=position_dodge(.9))

p_pupal_mass + scale_fill_manual(values = c("darkblue", "darkgreen", "red", "yellow"))


#day10_mass
data_summary_day10_mass <- function(data, varname = "day10_mass", groupnames = c("hostplant", "treatment")){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}


df_day10_mass <- data_summary_day10_mass(data, varname ="day10_mass", 
                       groupnames = c("hostplant", "treatment"))

#plot day10 mass x hostplant x treatment
p_day10_mass <- ggplot(df_day10_mass, aes(x=hostplant, y=day10_mass, fill=treatment)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=day10_mass-sd, ymax=day10_mass+sd), width=.2,
                position=position_dodge(.9))

p_day10_mass + scale_fill_manual(values = c("darkblue", "darkgreen", "red", "yellow"))


#larval_days
data_summary_larval_days <- function(data, varname = "larval_days", groupnames = c("hostplant", "treatment")){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}


df_larval_days <- data_summary_larval_days(data, varname ="larval_days", 
                       groupnames = c("hostplant", "treatment"))

#plot larval days x hostplant x treatment
p_larval_days <- ggplot(df_larval_days, aes(x=hostplant, y=larval_days, fill=treatment)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=larval_days-sd, ymax=larval_days+sd), width=.2,
                position=position_dodge(.9))

p_larval_days + scale_fill_manual(values = c("darkblue", "darkgreen", "red", "yellow"))


#re-organizing ANOVAs

#is fwl dependent on hostplant spp and/or treatment?
model <- aov(r2_adult$fwl ~ r2_adult$hostplant * r2_adult$treatment, 
             data = r2_adult)

#pull out hostplant data by species
alin_data <- r2_adult[which(r2_adult$hostplant == "alin"), names(r2_adult)
                      %in% c("larval_days", "day10_mass", "pupal_mass", "pupal_days", 
                             "fwl", "treatment", "plant_heat", "cat_heat")]

aang_data <- r2_adult[which(r2_adult$hostplant == "aang"), names(r2_adult)
                      %in% c("larval_days", "day10_mass", "pupal_mass", "pupal_days", 
                             "fwl", "treatment", "plant_heat", "cat_heat")]

asubu_data <- r2_adult[which(r2_adult$hostplant == "asubu"), names(r2_adult)
                       %in% c("larval_days", "day10_mass", "pupal_mass", "pupal_days", 
                              "fwl", "treatment", "plant_heat", "cat_heat")]

####ANOVAS for fwl
#one 2-way ANOVA where there are 4 treatments
model1_fwl <- aov(r2_adult$fwl ~ r2_adult$hostplant * r2_adult$treatment, 
             data = r2_adult)

summary(model1_fwl)

#three one-way ANOVAs where there are 4 treatments
model_alin_fwl <- aov(alin_data$fwl ~ alin_data$treatment, data = alin_data)
model_aang_fwl <- aov(aang_data$fwl ~ aang_data$treatment, data = aang_data)
model_asubu_fwl <- aov(asubu_data$fwl ~ asubu_data$treatment, data = asubu_data)

summary(model_alin_fwl)
summary(model_aang_fwl)
summary(model_asubu_fwl)

#three two-way ANOVAs where there are 2 treatments (plant heat and cat heat)
model_alin_2way_fwl <- aov(alin_data$fwl ~ alin_data$plant_heat * alin_data$cat_heat,
                           data = alin_data)

model_aang_2way_fwl <- aov(aang_data$fwl ~ aang_data$plant_heat * aang_data$cat_heat,
                           data = aang_data)

model_asubu_2way_fwl <- aov(asubu_data$fwl ~ asubu_data$plant_heat * asubu_data$cat_heat,
                            data = asubu_data)

summary(model_alin_2way_fwl)
summary(model_aang_2way_fwl)
summary(model_asubu_2way_fwl)

#one 3-way ANOVA with everything
model_3way_everything_fwl <- aov(r2_adult$fwl ~ r2_adult$cat_heat * r2_adult$plant_heat *
                                   r2_adult$hostplant, data = r2_adult)

summary(model_3way_everything_fwl)

# 
# #more box plots
# ggplot(data) +
#   aes(x = hostplant, y = fwl) +
#   geom_bar(stat = "identity")
# 
# 
# ##find all means and sds
# group_by(data, )
