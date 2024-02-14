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
# 
# #tests for normality
# plot(model, which = 2)
# 
# qqPlot(model$residuals, id = FALSE)

# #boxplot
# data <- r2_adult
# 
# ggplot(data) +
#   aes(x = data$hostplant, y = data$fwl, fill = data$treatment) +
#   geom_boxplot()
# 
# data %>%
#   filter(!is.na(data$treatment)) %>%
#   ggplot() +
#   aes(x = data$hostplant, y = data$fwl, fill = data$treatment) +
#   geom_boxplot()
# 
# ggplot(data) +
#   aes(x = data$treatment, y = data$fwl, fill = data$hostplant) +
#   geom_boxplot()

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

#calculate means and sds

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

# df_fwl$hostplant=as.factor(df_fwl$hostplant)
# head(df_fwl)

#plot fwl x hostplant x treatment

p <- ggplot(df_fwl, aes(x=hostplant, y=fwl, fill=treatment)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=fwl-sd, ymax=fwl+sd), width=.2,
                position=position_dodge(.9))

p + scale_fill_manual(values = c("darkblue", "darkgreen", "red", "yellow"))

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
                             "fwl", "treatment", "plant_heat", "cat_heat", "sex")]

aang_data <- r2_adult[which(r2_adult$hostplant == "aang"), names(r2_adult)
                      %in% c("larval_days", "day10_mass", "pupal_mass", "pupal_days", 
                             "fwl", "treatment", "plant_heat", "cat_heat", "sex")]

asubu_data <- r2_adult[which(r2_adult$hostplant == "asubu"), names(r2_adult)
                       %in% c("larval_days", "day10_mass", "pupal_mass", "pupal_days", 
                              "fwl", "treatment", "plant_heat", "cat_heat", "sex")]

####ANOVAS for fwl
#one 2-way ANOVA where there are 4 treatments
model1_fwl <- aov(r2_adult$fwl ~ r2_adult$hostplant * r2_adult$treatment + r2_adult$sex, 
             data = r2_adult)

summary(model1_fwl)

#three one-way ANOVAs where there are 4 treatments
model_alin_fwl <- aov(alin_data$fwl ~ alin_data$treatment + alin_data$sex, data = alin_data)
model_aang_fwl <- aov(aang_data$fwl ~ aang_data$treatment + aang_data$sex, data = aang_data)
model_asubu_fwl <- aov(asubu_data$fwl ~ asubu_data$treatment + asubu_data$sex, data = asubu_data)

summary(model_alin_fwl)
summary(model_aang_fwl)
summary(model_asubu_fwl)

#three two-way ANOVAs where there are 2 treatments (plant heat and cat heat)
model_alin_2way_fwl <- aov(alin_data$fwl ~ alin_data$plant_heat * alin_data$cat_heat +
                             alin_data$sex, data = alin_data)

model_aang_2way_fwl <- aov(aang_data$fwl ~ aang_data$plant_heat * aang_data$cat_heat +
                             aang_data$sex, data = aang_data)

model_asubu_2way_fwl <- aov(asubu_data$fwl ~ asubu_data$plant_heat * asubu_data$cat_heat +
                              asubu_data$sex, data = asubu_data)

summary(model_alin_2way_fwl)
summary(model_aang_2way_fwl)
summary(model_asubu_2way_fwl)

#one 3-way ANOVA with everything
model_3way_everything_fwl <- aov(r2_adult$fwl ~ r2_adult$cat_heat * r2_adult$plant_heat *
                                   r2_adult$hostplant + r2_adult$sex, data = r2_adult)

summary(model_3way_everything_fwl)


####ANOVAS for pupal days
#one 2-way ANOVA where there are 4 treatments
model1_pd <- aov(r2_adult$pupal_days ~ r2_adult$hostplant * r2_adult$treatment + r2_adult$sex, 
                  data = r2_adult)

summary(model1_pd)

#three one-way ANOVAs where there are 4 treatments
model_alin_pd <- aov(alin_data$pupal_days ~ alin_data$treatment + alin_data$sex, data = alin_data)
model_aang_pd <- aov(aang_data$pupal_days ~ aang_data$treatment + aang_data$sex, data = aang_data)
model_asubu_pd <- aov(asubu_data$pupal_days ~ asubu_data$treatment + asubu_data$sex, data = asubu_data)

summary(model_alin_pd)
summary(model_aang_pd)
summary(model_asubu_pd)

#three two-way ANOVAs where there are 2 treatments (plant heat and cat heat)
model_alin_2way_pd <- aov(alin_data$pupal_days ~ alin_data$plant_heat * alin_data$cat_heat +
                             alin_data$sex, data = alin_data)

model_aang_2way_pd <- aov(aang_data$pupal_days ~ aang_data$plant_heat * aang_data$cat_heat +
                             aang_data$sex, data = aang_data)

model_asubu_2way_pd <- aov(asubu_data$pupal_days ~ asubu_data$plant_heat * asubu_data$cat_heat +
                              asubu_data$sex, data = asubu_data)

summary(model_alin_2way_pd)
summary(model_aang_2way_pd)
summary(model_asubu_2way_pd)

#one 3-way ANOVA with everything
model_3way_everything_pd <- aov(r2_adult$pupal_days ~ r2_adult$cat_heat * r2_adult$plant_heat *
                                   r2_adult$hostplant + r2_adult$sex, data = r2_adult)

summary(model_3way_everything_pd)


####ANOVAS for pupal mass
#one 2-way ANOVA where there are 4 treatments
model1_pm <- aov(r2_adult$pupal_mass ~ r2_adult$hostplant * r2_adult$treatment + r2_adult$sex, 
                 data = r2_adult)

summary(model1_pm)

#three one-way ANOVAs where there are 4 treatments
model_alin_pm <- aov(alin_data$pupal_mass ~ alin_data$treatment + alin_data$sex, data = alin_data)
model_aang_pm <- aov(aang_data$pupal_mass ~ aang_data$treatment + aang_data$sex, data = aang_data)
model_asubu_pm <- aov(asubu_data$pupal_mass ~ asubu_data$treatment + asubu_data$sex, data = asubu_data)

summary(model_alin_pm)
summary(model_aang_pm)
summary(model_asubu_pm)

#three two-way ANOVAs where there are 2 treatments (plant heat and cat heat)
model_alin_2way_pm <- aov(alin_data$pupal_mass ~ alin_data$plant_heat * alin_data$cat_heat +
                            alin_data$sex, data = alin_data)

model_aang_2way_pm <- aov(aang_data$pupal_mass ~ aang_data$plant_heat * aang_data$cat_heat +
                            aang_data$sex, data = aang_data)

model_asubu_2way_pm <- aov(asubu_data$pupal_mass ~ asubu_data$plant_heat * asubu_data$cat_heat +
                             asubu_data$sex, data = asubu_data)

summary(model_alin_2way_pm)
summary(model_aang_2way_pm)
summary(model_asubu_2way_pm)

#one 3-way ANOVA with everything
model_3way_everything_pm <- aov(r2_adult$pupal_mass ~ r2_adult$cat_heat * r2_adult$plant_heat *
                                  r2_adult$hostplant + r2_adult$sex, data = r2_adult)

summary(model_3way_everything_pm)

####ANOVAS for day 10 mass
#one 2-way ANOVA where there are 4 treatments
model1_day10m <- aov(r2_adult$day10_mass ~ r2_adult$hostplant * r2_adult$treatment + r2_adult$sex, 
                 data = r2_adult)

summary(model1_day10m)

#three one-way ANOVAs where there are 4 treatments
model_alin_day10m <- aov(alin_data$day10_mass ~ alin_data$treatment + alin_data$sex, data = alin_data)
model_aang_day10m <- aov(aang_data$day10_mass ~ aang_data$treatment + aang_data$sex, data = aang_data)
model_asubu_day10m <- aov(asubu_data$day10_mass ~ asubu_data$treatment + asubu_data$sex, data = asubu_data)

summary(model_alin_day10m)
summary(model_aang_day10m)
summary(model_asubu_day10m)

#three two-way ANOVAs where there are 2 treatments (plant heat and cat heat)
model_alin_2way_day10m <- aov(alin_data$day10_mass ~ alin_data$plant_heat * alin_data$cat_heat +
                            alin_data$sex, data = alin_data)

model_aang_2way_day10m <- aov(aang_data$day10_mass ~ aang_data$plant_heat * aang_data$cat_heat +
                            aang_data$sex, data = aang_data)

model_asubu_2way_day10m <- aov(asubu_data$day10_mass ~ asubu_data$plant_heat * asubu_data$cat_heat +
                             asubu_data$sex, data = asubu_data)

summary(model_alin_2way_day10m)
summary(model_aang_2way_day10m)
summary(model_asubu_2way_day10m)

#one 3-way ANOVA with everything
model_3way_everything_day10m <- aov(r2_adult$day10_mass ~ r2_adult$cat_heat * r2_adult$plant_heat *
                                  r2_adult$hostplant + r2_adult$sex, data = r2_adult)

summary(model_3way_everything_day10m)

####ANOVAS for larval days
#one 2-way ANOVA where there are 4 treatments
model1_ld <- aov(r2_adult$larval_days ~ r2_adult$hostplant * r2_adult$treatment + r2_adult$sex, 
                     data = r2_adult)

summary(model1_ld)

#three one-way ANOVAs where there are 4 treatments
model_alin_ld <- aov(alin_data$larval_days ~ alin_data$treatment + alin_data$sex, data = alin_data)
model_aang_ld <- aov(aang_data$larval_days ~ aang_data$treatment + aang_data$sex, data = aang_data)
model_asubu_ld <- aov(asubu_data$larval_days ~ asubu_data$treatment + asubu_data$sex, data = asubu_data)

summary(model_alin_ld)
summary(model_aang_ld)
summary(model_asubu_ld)

#three two-way ANOVAs where there are 2 treatments (plant heat and cat heat)
model_alin_2way_ld <- aov(alin_data$larval_days ~ alin_data$plant_heat * alin_data$cat_heat +
                                alin_data$sex, data = alin_data)

model_aang_2way_ld <- aov(aang_data$larval_days ~ aang_data$plant_heat * aang_data$cat_heat +
                                aang_data$sex, data = aang_data)

model_asubu_2way_ld <- aov(asubu_data$larval_days ~ asubu_data$plant_heat * asubu_data$cat_heat +
                                 asubu_data$sex, data = asubu_data)

summary(model_alin_2way_ld)
summary(model_aang_2way_ld)
summary(model_asubu_2way_ld)

#one 3-way ANOVA with everything
model_3way_everything_ld <- aov(r2_adult$larval_days ~ r2_adult$cat_heat * r2_adult$plant_heat *
                                      r2_adult$hostplant + r2_adult$sex, data = r2_adult)

summary(model_3way_everything_ld)


# 
# #more box plots
# ggplot(data) +
#   aes(x = hostplant, y = fwl) +
#   geom_bar(stat = "identity")
# 
# 
# ##find all means and sds
# group_by(data, )
