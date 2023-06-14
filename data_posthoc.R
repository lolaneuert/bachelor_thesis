# load data, transform treatment into factor, calculate post-hoc tests for each variable 

getwd()
setwd("C:/RStudio/bachelorarbeit")
getwd()

# install packages
library(dplyr)   
library(stats)
library(factoextra)
library(tidyverse)
library(ggplot2)
library(car)
library(ggpubr)
library(broom)
library(AICcmodavg)
install.packages("DescTools")
library(DescTools)

#load data
data_log <- read.table("C:/RStudio/bachelorarbeit/data_log.csv", sep = ",", dec = ".", header = TRUE, na.strings = "")

treatment_factor <- factor(data_log$treatment) # transform treatment column into factor variable (can contain string and integer as categorization of data, stores it as levels)
data_log2<- data.frame(treatment_factor, data_log[,2:8]) # put factor-treatment together with other log10 transformed data
print(is.factor(data_log2$treatment))

str(data_log2)

# calculate Tukey post-hoc (this method is best to use when the sample size of each group is equal, R actually calculates Tukey-Kramer test with this formula, which is better suited for unequal sample-sizes)
# for P-adj <0.05: significant difference between treatments
anova_P_conc <- (aov(data_log2$P_conc_inlet~data_log2$treatment_factor)) # anova as list needed
TukeyHSD(anova_P_conc, conf.level=.95) # calculate Tukey test using anova list for each variable
#Fit: aov(formula = data_log2$P_conc_inlet ~ data_log2$treatment_factor)
#$`data_log2$treatment_factor`
#         diff       lwr       upr     p adj
#P1-P0 0.7363806 0.4154148 1.0573465 0.0005557
#P2-P0 1.2283870 0.9074212 1.5493529 0.0000199
#P3-P0 1.6939427 1.3350919 2.0527934 0.0000047
#P2-P1 0.4920064 0.1710406 0.8129722 0.0060641
#P3-P1 0.9575620 0.5987113 1.3164127 0.0002129
#P3-P2 0.4655556 0.1067049 0.8244063 0.0147202

DunnettTest(x=data_log2$P_conc_inlet, g = data_log2$treatment_factor) #calculate dunnett's post hoc test, since P0 is control this is better suited than tukeys
# Dunnett's test for comparing several treatments with a control :  
#    95% family-wise confidence level

#$P0
#           diff    lwr.ci   upr.ci    pval    
#P1-P0 0.7363806 0.4463734 1.026388 0.00049 ***
#P2-P0 1.2283870 0.9383798 1.518394 6.4e-06 ***
#P3-P0 1.6939427 1.3697047 2.018181 1.4e-05 ***

#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

anova_P_entrap<- (aov(data_log2$P_entrap_eff_biofilm~data_log2$treatment_factor)) # anova as list needed
TukeyHSD(anova_P_entrap, conf.level=.95) # calculate Tukey test using anova list for each variable
# Fit: aov(formula = data_log2$P_entrap_eff_biofilm ~ data_log2$treatment_factor)
#$`data_log2$treatment_factor`
#           diff        lwr       upr     p adj
#P1-P0 -0.07639801 -0.9672205 0.8144245 0.9913166
#P2-P0  0.15794285 -0.7328797 1.0487654 0.9329199
#P3-P0  0.19450136 -0.8014685 1.1904712 0.9135268
#P2-P1  0.23434086 -0.6564817 1.1251634 0.8197439
#P3-P1  0.27089936 -0.7250705 1.2668692 0.8053694
#P3-P2  0.03655851 -0.9594114 1.0325284 0.9992950


anova_APA <- (aov(data_log2$APA~data_log2$treatment_factor)) # anova as list needed
TukeyHSD(anova_APA, conf.level=.95) # calculate Tukey test using anova list for each variable
#Fit: aov(formula = data_log2$APA ~ data_log2$treatment_factor)
#$`data_log2$treatment_factor`
#          diff        lwr         upr     p adj
#P1-P0 -0.05701836 -0.3839385  0.26990175 0.9357937
#P2-P0 -0.23348964 -0.5604098  0.09343047 0.1722097
#P3-P0 -0.40891593 -0.7744237 -0.04340813 0.0303010
#P2-P1 -0.17647128 -0.5033914  0.15044883 0.3530362
#P3-P1 -0.35189756 -0.7174054  0.01361024 0.0586598
#P3-P2 -0.17542628 -0.5409341  0.19008152 0.4415219

DunnettTest(x=data_log2$APA, g = data_log2$treatment_factor)
#             diff     lwr.ci      upr.ci   pval    
#P1-P0 -0.05701836 -0.3512405  0.23720381 0.8929    
#P2-P0 -0.23348964 -0.5277118  0.06073253 0.1178    
#P3-P0 -0.40891593 -0.7378663 -0.07996554 0.0193 *  
  

anova_Poly_p <- (aov(data_log2$Poly_p~data_log2$treatment_factor)) # anova as list needed
TukeyHSD(anova_Poly_p, conf.level=.95) # calculate Tukey test using anova list for each variable
#Fit: aov(formula = data_log2$Poly_p ~ data_log2$treatment_factor)
#$`data_log2$treatment_factor`
#           diff        lwr        upr     p adj
#P1-P0 -0.056576157 -0.1914236 0.07827127 0.5429973
#P2-P0 -0.029682061 -0.1645295 0.10516537 0.8827057
#P3-P0 -0.031106877 -0.1818709 0.11965713 0.9003815
#P2-P1  0.026894096 -0.1079533 0.16174152 0.9086906
#P3-P1  0.025469280 -0.1252947 0.17623329 0.9410540
#P3-P2 -0.001424816 -0.1521888 0.14933919 0.9999879


anova_P_biofilm <- (aov(data_log2$P_biofilm~data_log2$treatment_factor)) # anova as list needed
TukeyHSD(anova_P_biofilm, conf.level=.95) # calculate Tukey test using anova list for each variable
# Fit: aov(formula = data_log2$P_biofilm ~ data_log2$treatment_factor)
#$`data_log2$treatment_factor`
#         diff         lwr       upr     p adj
#P1-P0  0.27980498  0.04764828 0.5119617 0.0212527
#P2-P0  0.24485713  0.01270043 0.4770138 0.0396277
#P3-P0  0.38549545  0.12593636 0.6450545 0.0072128
#P2-P1 -0.03494785 -0.26710455 0.1972089 0.9569245
#P3-P1  0.10569047 -0.15386862 0.3652496 0.5649114
#P3-P2  0.14063832 -0.11892077 0.4001974 0.3502653

DunnettTest(x=data_log2$P_biofilm, g = data_log2$treatment_factor)
#diff     lwr.ci    upr.ci   pval    
#P1-P0 0.2798050 0.07009986 0.4895101 0.0130 *  
#P2-P0 0.2448571 0.03515202 0.4545623 0.0251 *  
#P3-P0 0.3854955 0.15103800 0.6199529 0.0043 ** 
  
anova_P_balance <- (aov(data_log2$P_balance~data_log2$treatment_factor)) # anova as list needed
TukeyHSD(anova_P_balance, conf.level=.95) # calculate Tukey test using anova list for each variable
# Fit: aov(formula = data_log2$P_balance ~ data_log2$treatment_factor)
#$`data_log2$treatment_factor`
#          diff          lwr        upr     p adj
#P1-P0 0.01967475 -0.024374616 0.06372412 0.4961851
#P2-P0 0.03451252 -0.009536850 0.07856189 0.1276873
#P3-P0 0.05882495  0.009576262 0.10807365 0.0222035
#P2-P1 0.01483777 -0.029211604 0.05888714 0.6925942
#P3-P1 0.03915020 -0.010098493 0.08839889 0.1214922
#P3-P2 0.02431243 -0.024936259 0.07356113 0.4200785

DunnettTest(x=data_log2$P_balance, g = data_log2$treatment_factor)
#            diff       lwr.ci     upr.ci   pval    
#P1-P0 0.01967475 -0.020006771 0.05935628 0.3853    
#P2-P0 0.03451252 -0.005169005 0.07419405 0.0855 .  
#P3-P0 0.05882495  0.014459661 0.10319025 0.0139 *  
  
anova_P_remove <- (aov(data_log2$P_remove_eff~data_log2$treatment_factor)) # anova as list needed
TukeyHSD(anova_P_remove, conf.level=.95) # calculate Tukey test using anova list for each variable
# Fit: aov(formula = data_log2$P_remove_eff ~ data_log2$treatment_factor)
#$`data_log2$treatment_factor`
#           diff         lwr        upr     p adj
#P1-P0  0.07832161 -0.01654108 0.17318429 0.1063169
#P2-P0  0.06281058 -0.03205210 0.15767327 0.2148865
#P3-P0  0.05197178 -0.05408793 0.15803149 0.4257095
#P2-P1 -0.01551102 -0.11037371 0.07935166 0.9460228
#P3-P1 -0.02634983 -0.13240953 0.07970988 0.8423929
#P3-P2 -0.01083880 -0.11689851 0.09522090 0.9855648




