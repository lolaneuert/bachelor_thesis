# load data, transform treatment column into factor, calculate one-factor Anova for each dependent variable, 
# calculate Manova for all dependent variables, always dependent on treatment, calculate Welch test

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
install.packages("AICcmodavg")
library(AICcmodavg)

#load data
data_log <- read.table("C:/RStudio/bachelorarbeit/data_log.csv", sep = ",", dec = ".", header = TRUE, na.strings = "")

treatment_factor <- factor(data_log$treatment) # transform treatment column into factor variable (can contain string and integer as categorization of data, stores it as levels)
data_log2<- data.frame(treatment_factor, data_log[,2:8]) # put factor-treatment together with other log10 transformed data
print(is.factor(data_log2$treatment))

str(data_log2)

# H0: µP0 = µP1 = µP2 = µP3, H1: µi =/ µi for at least one pair

#calculate a one-way/factor ANOVA for each dependent variable, treatment=independent variable
summary(aov(data_log2$P_conc_inlet~data_log2$treatment_factor))
#           Df Sum Sq Mean Sq F value   Pr(>F)    
# treatment 3  4.060  1.3533   95.96 4.75e-06 ***
# Residuals 7  0.099  0.0141                     
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# very small p-value (***=0), therefore significant: H0 can be rejected, treatment has strong influence on P-concentration auf 

summary(aov(data_log2$P_entrap_eff_biofilm~data_log2$treatment_factor))
#                            Df Sum Sq Mean Sq F value Pr(>F)
#data_log2$treatment_factor  3 0.1315 0.04384   0.404  0.755
#Residuals                   7 0.7605 0.10864    

summary(aov(data_log2$APA~data_log2$treatment_factor))
#                            Df Sum Sq Mean Sq F value Pr(>F)  
#data_log2$treatment_factor  3 0.2483 0.08276   5.657 0.0275 *
#Residuals                   7 0.1024 0.01463                 
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

summary(aov(data_log2$Poly_p~data_log2$treatment_factor))
#                            Df   Sum Sq  Mean Sq F value Pr(>F)
#data_log2$treatment_factor  3 0.004814 0.001605   0.645  0.611
#Residuals                   7 0.017425 0.002489    

summary(aov(data_log2$P_biofilm~data_log2$treatment_factor))
#                            Df  Sum Sq Mean Sq F value  Pr(>F)   
#data_log2$treatment_factor  3 0.21205 0.07068    9.58 0.00713 **
#Residuals                   7 0.05165 0.00738                   
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

summary(aov(data_log2$P_balance~data_log2$treatment_factor))
#                            Df   Sum Sq   Mean Sq F value Pr(>F)  
#data_log2$treatment_factor  3 0.004517 0.0015058   5.669 0.0274 *
#Residuals                   7 0.001859 0.0002656                 
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

summary(aov(data_log2$P_remove_eff~data_log2$treatment_factor))
#                            Df   Sum Sq  Mean Sq F value Pr(>F)
#data_log2$treatment_factor  3 0.010360 0.003453   2.803  0.118
#Residuals                   7 0.008623 0.001232  



# calculate a MANOVA, as it is able to calculate the anova for multiple dependent variables, treatment=independent variable
manova_data <- manova(cbind(P_conc_inlet, P_entrap_eff_biofilm, APA, Poly_p, P_biofilm, P_balance, P_remove_eff) ~ treatment_factor, data = data_log2)
summary(manova_data)
#                   Df Pillai approx F num Df den Df Pr(>F)
# treatment_factor  3 2.5163   2.2297     21      9 0.1076
# Residuals         7 

summary.aov(manova_data)

# Response P_conc_inlet :
#                  Df Sum Sq Mean Sq F value    Pr(>F)
#treatment_factor  3 4.0598  1.3533  95.957 4.755e-06
#Residuals         7 0.0987  0.0141                  

#treatment_factor ***
# Residuals           
# Signif. codes:   0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Response P_entrap_eff_biofilm :
#                  Df  Sum Sq  Mean Sq F value Pr(>F)
#treatment_factor  3 0.13151 0.043837  0.4035 0.7552
#Residuals         7 0.76045 0.108636               

#Response APA :
#                  Df  Sum Sq  Mean Sq F value  Pr(>F)
#treatment_factor  3 0.24828 0.082761  5.6565 0.02753
#Residuals         7 0.10242 0.014631                

#treatment_factor *
# Residuals         
#  Signif. codes:   0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Response Poly_p :
#                  Df    Sum Sq   Mean Sq F value
#treatment_factor  3 0.0048142 0.0016048  0.6447
#Residuals         7 0.0174251 0.0024893        
#                 Pr(>F)
#treatment_factor 0.6105
#Residuals              

#Response P_biofilm :
#                  Df   Sum Sq  Mean Sq F value
#treatment_factor  3 0.212049 0.070683  9.5799
#Residuals         7 0.051648 0.007378        
#                  Pr(>F)   
#treatment_factor 0.007126 **
#  Residuals                   
#  Signif. codes:   0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Response P_balance :
#                  Df    Sum Sq    Mean Sq F value
#treatment_factor  3 0.0045173 0.00150577  5.6688
#Residuals         7 0.0018594 0.00026563        
#                 Pr(>F)  
#treatment_factor 0.02739 *
#  Residuals                 
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Response P_remove_eff :
#              Df    Sum Sq   Mean Sq F value
# treatment_factor  3 0.0103598 0.0034533  2.8031
# Residuals         7 0.0086235 0.0012319        
#                  Pr(>F)
# treatment_factor 0.1181
# Residuals   
#

#calculate Welch Test, which doesn't assume equal variances

oneway.test(P_conc_inlet~treatment_factor, data = data_log2)
# data:  P_conc_inlet and treatment_factor
#F = 767.85, num df = 3.0000, denom df = 2.7197, p-value = 0.0001713

oneway.test(P_entrap_eff_biofilm~treatment_factor, data = data_log2)
# data:  P_entrap_eff_biofilm and treatment_factor
#F = 1.8541, num df = 3.0000, denom df = 3.6769, p-value = 0.2875

oneway.test(APA~treatment_factor, data = data_log2)
# data:  APA and treatment_factor
#F = 8.0485, num df = 3.0000, denom df = 2.8735, p-value = 0.06476

oneway.test(Poly_p~treatment_factor, data = data_log2)
# data:  Poly_p and treatment_factor
#F = 0.60078, num df = 3.0000, denom df = 3.0501, p-value = 0.6565

oneway.test(P_biofilm~treatment_factor, data = data_log2)
# data:  P_biofilm and treatment_factor
#F = 20.193, num df = 3.0000, denom df = 3.0313, p-value = 0.01665

oneway.test(P_balance~treatment_factor, data = data_log2)
# data:  P_balance and treatment_factor
#F = 37.657, num df = 3.0000, denom df = 3.3265, p-value = 0.004717

oneway.test(P_remove_eff~treatment_factor, data = data_log2)
# data:  P_remove_eff and treatment_factor
#F = 2.1644, num df = 3.0000, denom df = 3.5629, p-value = 0.2488