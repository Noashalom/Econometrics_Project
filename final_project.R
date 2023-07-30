
# setup -------------------------------------------------------------------

rm(list=ls()) 
gc()

library(tidyverse)
library(modelsummary)
library(car)
library(sandwich)
library(lmtest)
library(lfe)
library(modelsummary)

setwd("C:/Users/User/Noa degree/Eco/project")

options(scipen=999)
options(na.action=na.exclude)


# Part B ------------------------------------------------------------------

our_data <- read.csv("C:/Users/User/Noa degree/YEAR 2/Eco/project/term_paper_data_did.csv")

#Data Organize

our_data$f_level <- factor(our_data$level,  levels = c(1,2,3,4,5,6,7))
levels(our_data$f_level) <- c('1','2','3','4','5','6','7')

# Q6 ----------------------------------------------------------------------


subset_data48 <- our_data[our_data$d53==0 & our_data$year==85, ]

mean_lnearn_reform_48 <- mean(subset_data48$lnearn[subset_data48$treat_muni == 1])
mean_yearsch_reform_48 <- mean(subset_data48$yearsch[subset_data48$treat_muni == 1])

mean_lnearn_unreform_48 <- mean(subset_data48$lnearn[subset_data48$treat_muni == 0])
mean_yearsch_unreform_48 <- mean(subset_data48$yearsch[subset_data48$treat_muni == 0])

ref_yearsch_48 <- lm(yearsch ~ treat_muni, data = subset_data48)
ref_lnearn_48 <- lm(lnearn ~ treat_muni, data = subset_data48)

summary(ref_yearsch_48)
summary(ref_lnearn_48)


# Q8 ----------------------------------------------------------------------


subset_data53 <- our_data[our_data$d53==1 & our_data$year==85, ]

mean_lnearn_reform_53 <- mean(subset_data53$lnearn[subset_data53$treat_muni == 1])
mean_yearsch_reform_53 <- mean(subset_data53$yearsch[subset_data53$treat_muni == 1])

mean_lnearn_unreform_53 <- mean(subset_data53$lnearn[subset_data53$treat_muni == 0])
mean_yearsch_unreform_53 <- mean(subset_data53$yearsch[subset_data53$treat_muni == 0])

ref_yearsch_53 <- lm(yearsch ~ treat_muni, data = subset_data53)
ref_lnearn_53 <- lm(lnearn ~ treat_muni, data = subset_data53)

summary(ref_yearsch_53)
summary(ref_lnearn_53)


# Q10 ---------------------------------------------------------------------

reg_10 <- lm(lnearn ~ yearsch + treat_muni + female + hiab3 + hife, data = subset_data48)
summary(reg_10)


# Q12 ---------------------------------------------------------------------

reg_12 <- lm(lnearn ~ f_level + treat_muni + female + hiab3 + hife, data = subset_data48)
summary(reg_12)


# Q14 ---------------------------------------------------------------------

subset_data85 <- our_data[our_data$year==85, ]

mean_yearsch_48_reform <- mean(subset_data85$yearsch[subset_data85$treat_muni==1 & subset_data85$d53==0])
mean_yearsch_48_unreform <- mean(subset_data85$yearsch[subset_data85$treat_muni==0 & subset_data85$d53==0])
mean_yearsch_53_reform <- mean(subset_data85$yearsch[subset_data85$treat_muni==1 & subset_data85$d53==1])
mean_yearsch_53_unreform <- mean(subset_data85$yearsch[subset_data85$treat_muni==0 & subset_data85$d53==1])


# Q15 ---------------------------------------------------------------------

reg_15 <- lm(yearsch ~ treat_muni + d53 + treat_muni:d53, data = subset_data85)
summary(reg_15)


# Q17 ---------------------------------------------------------------------

subset_data85$residual_sq <- residuals(reg_15)^2
BP_model <- lm(residual_sq ~ treat_muni + d53 + treat_muni:d53, data = subset_data85)
linearHypothesis(BP_model, c('treat_muni=0', 'd53=0', 'treat_muni:d53 = 0'))

#re-estimate the model with White's robust standard errors

coeftest(reg_15, vcov = vcovHC(reg_15, "HC1"))


# Q18 ---------------------------------------------------------------------

reg_18 <- lm(yearsch ~ treat_muni + d53 + treat_muni:d53 + female + hiab3 + hife, data = subset_data85)
summary(reg_18)

subset_data85$residual_18_sq <- residuals(reg_18)^2
BP_model2 <- lm(residual_18_sq ~ treat_muni + d53 + treat_muni:d53 + female + hiab3 + hife, data = subset_data85)
linearHypothesis(BP_model2, c('treat_muni=0', 'd53=0', 'treat_muni:d53 = 0', 'female=0','hiab3=0','hife=0' ))

coeftest(reg_18, vcov = vcovHC(reg_18, "HC1"))

# Q19 ---------------------------------------------------------------------

#f test for the new features:

linearHypothesis(reg_18, c('female=0','hiab3=0','hife=0'), white.adjust = "hc1")


# Q22 ---------------------------------------------------------------------

reg_22 <- felm(lnearn ~ treat_muni + d53 + treat_muni:d53 + female + hiab3 + hife + f_level | year | 0 | id + fk, data = our_data)

summary(reg_22)

# Q23 ---------------------------------------------------------------------

subset_data_female <- our_data[our_data$female ==1, ]

subset_data_male <- our_data[our_data$female ==0, ]

reg_23_female <- felm(lnearn ~ treat_muni + d53 + treat_muni:d53 + hiab3 + hife + f_level | year | 0 | id + fk, data = subset_data_female)
reg_23_male <- felm(lnearn ~ treat_muni + d53 + treat_muni:d53 + hiab3 + hife + f_level | year | 0 |id + fk, data = subset_data_male)

#Now with father education:

subset_data_female_father_high <- our_data[our_data$female ==1 & our_data$hife == 1, ]
subset_data_male_father_high <- our_data[our_data$female ==0 & our_data$hife == 1, ]

subset_data_female_father_low <- our_data[our_data$female ==1 & our_data$hife == 0, ]
subset_data_male_father_low <- our_data[our_data$female ==0 & our_data$hife == 0, ]

reg_23_female_father_high <- felm(lnearn ~ treat_muni + d53 + treat_muni:d53 + hiab3 + f_level | year | 0 | id + fk, data = subset_data_female_father_high)
reg_23_male_father_high <- felm(lnearn ~ treat_muni + d53 + treat_muni:d53 + hiab3 + f_level | year | 0 | id + fk, data = subset_data_male_father_high)

reg_23_female_father_low <- felm(lnearn ~ treat_muni + d53 + treat_muni:d53 + hiab3 + f_level | year | 0 | id + fk, data = subset_data_female_father_low)
reg_23_male_father_low <- felm(lnearn ~ treat_muni + d53 + treat_muni:d53 + hiab3 + f_level | year | 0 | id + fk, data = subset_data_male_father_low)

#summary for the models

model_list <- list(reg_23_female, reg_23_male, reg_23_female_father_high, reg_23_male_father_high, reg_23_female_father_low, reg_23_male_father_low)

modelsummary(model_list)

