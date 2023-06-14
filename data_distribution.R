
# test normal distribution with histogramms, with QQplots, with Shapiro-Wilk Test, with Kolmogorov-Smirnov Test
# test homogeneity of variance of data, using scatterplots (ggplots with points/boxplots), with Bartlett Test, with Levene's Test, with Fligner-Killeen's Test 

#set working directory
getwd()
setwd("C:/RStudio/bachelorarbeit")
getwd()

# install packages
install.packages("dplyr") 
install.packages("stats")
install.packages("factoextra")
library(dplyr)   
library(stats)
library(factoextra)

names(data_log) <-c("treatment", "P_conc_inlet", "P_entrap_eff_biofilm", "APA", "Poly_p", "P_biofilm", "P_balance", "P_remove_eff")


#test normal distribution of data: different methods (what if different results?)

# test normal distribution with histogramms, using raw and log10 transformed data
png("histogramm_P_conc_raw_log.png", width=800, height=1000)
par(mfrow=c(1,2))
hist(data_pos$P_conc_inlet, col='light blue', main='raw')
hist(data_log$P_conc_inlet, col='dark blue', main='log')
mtext("Histogramm P concentration raw vs log10", side=3, line=-1, outer= TRUE)
dev.off()

png("histogramm_P_entrapment_raw_log.png", width=800, height=1000)
par(mfrow=c(1,2))
hist(data_pos$P_entrap_eff_biofilm, col='blue', main='raw')
hist(data_log$P_entrap_eff_biofilm, col='purple', main='log')
mtext("Histogramm P entrapment efficiency raw vs log10", side=3, line=-1, outer= TRUE)
dev.off()

png("histogramm_APA_raw_log.png", width=800, height=1000)
par(mfrow=c(1,2))
hist(data_pos$APA, col='red', main='raw')
hist(data_log$APA, col='dark red', main='log')
mtext("Histogramm APA raw vs log10", side=3, line=-1, outer= TRUE)
dev.off()

png("histogramm_Poly_p_raw_log.png", width=800, height=1000)
par(mfrow=c(1,2))
hist(data_pos$Poly_p, col='orange', main='raw')
hist(data_log$Poly_p, col='red', main='log')
mtext("Histogramm Polyphosphate raw vs log10", side=3, line=-1, outer= TRUE)
dev.off()

png("histogramm_P_biofilm_raw_log.png", width=800, height=1000)
par(mfrow=c(1,2))
hist(data_pos$P_biofilm, col='yellow', main='raw')
hist(data_log$P_biofilm, col='orange', main='log')
mtext("Histogramm P biofilm raw vs log10", side=3, line=-1, outer= TRUE)
dev.off()

png("histogramm_P_balance_raw_log.png", width=800, height=1000)
par(mfrow=c(1,2))
hist(data_pos$P_balance_positive, col='light green', main='raw')
hist(data_log$P_balance, col='dark green', main='log')
mtext("Histogramm P balance raw vs log10", side=3, line=-1, outer= TRUE)
dev.off()

png("histogramm_P_removal_raw_log.png", width=800, height=1000)
par(mfrow=c(1,2))
hist(data_pos$P_remove_eff_positive, col='dark green', main='raw')
hist(data_log$P_remove_eff, col='steelblue', main='log')
mtext("Histogramm P removal raw vs log10", side=3, line=-1, outer= TRUE)
dev.off()

# test normal distribution with QQ-Plots
png("QQplot_P_conc_raw_log.png", width=800, height=1000)
par(mfrow=c(1,2))
qqnorm(data_pos$P_conc_inlet, main='raw')
qqline(data_pos$P_conc_inlet)
qqnorm(data_log$P_conc_inlet, main='log')
qqline(data_log$P_conc_inlet)
mtext("QQplot:P concentration raw vs. log10", side=3, line=-1, outer= TRUE)
dev.off()

png("QQplot_P_entrapment_raw_log.png", width=800, height=1000)
par(mfrow=c(1,2))
qqnorm(data_pos$P_entrap_eff_biofilm, main='raw')
qqline(data_pos$P_entrap_eff_biofilm)
qqnorm(data_log$P_entrap_eff_biofilm, main='log')
qqline(data_log$P_entrap_eff_biofilm)
mtext("QQplot: P entrapment raw vs. log10", side=3, line=-1, outer= TRUE)
dev.off()

png("QQplot_APA_raw_log.png", width=800, height=1000)
par(mfrow=c(1,2))
qqnorm(data_pos$APA, main='raw')
qqline(data_pos$APA)
qqnorm(data_log$APA, main='log')
qqline(data_log$APA)
mtext("QQplot: APA raw vs. log10", side=3, line=-1, outer= TRUE)
dev.off()

png("QQplot_Poly_p_raw_log.png", width=800, height=1000)
par(mfrow=c(1,2))
qqnorm(data_pos$Poly_p, main='raw')
qqline(data_pos$Poly_p)
qqnorm(data_log$Poly_p, main='log')
qqline(data_log$Poly_p)
mtext("QQplot: Poly_p raw vs. log10", side=3, line=-1, outer= TRUE)
dev.off()

png("QQplot_P_biofilm_raw_log.png", width=800, height=1000)
par(mfrow=c(1,2))
qqnorm(data_pos$P_biofilm, main='raw')
qqline(data_pos$P_biofilm)
qqnorm(data_log$P_biofilm, main='log')
qqline(data_log$P_biofilm)
mtext("QQplot: P biofilm raw vs. log10", side=3, line=-1, outer= TRUE)
dev.off()

png("QQplot_P_balance_raw_log.png", width=800, height=1000)
par(mfrow=c(1,2))
qqnorm(data_pos$P_balance_positive, main='raw')
qqline(data_pos$P_balance_positive)
qqnorm(data_log$P_balance, main='log')
qqline(data_log$P_balance)
mtext("QQplot: P balance raw vs. log10", side=3, line=-1, outer= TRUE)
dev.off()

png("QQplot_P_removal_raw_log.png", width=800, height=1000)
par(mfrow=c(1,2))
qqnorm(data_pos$P_remove_eff_positive, main='raw')
qqline(data_pos$P_remove_eff_positive)
qqnorm(data_log$P_remove_eff, main='log')
qqline(data_log$P_remove_eff)
mtext("QQplot: P removal raw vs. log10", side=3, line=-1, outer= TRUE)
dev.off()

# test normal distribution with Shapiro-Wilk Test (if p is no less than 0.05, data is normally distributed))
shapiro.test(data_log$P_conc_inlet)            # W= 0.90895, p-value = 0.237
shapiro.test(data_log$P_entrap_eff_biofilm)    # W = 0.93337, p-value = 0.4459
shapiro.test(data_log$APA)                     # W = 0.88498, p-value = 0.1203
shapiro.test(data_log$Poly_p)                  # W = 0.93914, p-value = 0.5104
shapiro.test(data_log$P_biofilm)               # W = 0.90541, p-value = 0.215
shapiro.test(data_log$P_balance_positive)      # W = 0.88653, p-value = 0.1258
shapiro.test(data_log$P_remove_eff_positive)   # W = 0.84819, p-value = 0.04042   --> this one isn't normally distributed!

# test normal distribution with Kolmogorov-Smirnov Test (if p is no less than 0.05, data is normally distributed)
ks.test(data_log$P_conc_inlet, 'pnorm')            # D = 0.81676, p-value = 1.645e-08
#alternative hypothesis: two-sided
ks.test(data_log$P_entrap_eff_biofilm, 'pnorm')    # D = 0.37866, p-value = 0.06314
#alternative hypothesis: two-sided
ks.test(data_log$APA, 'pnorm')                     # D = 0.9116, p-value = 5.151e-12
#alternative hypothesis: two-sided
ks.test(data_log$Poly_p, 'pnorm')                  # D = 0.7609, p-value = 3.781e-07
#alternative hypothesis: two-sided
ks.test(data_log$P_biofilm, 'pnorm')               # warning! no ties should be present for ks.test??
ks.test(data_log$P_balance_positive, 'pnorm')      # D = 0.985, p-value < 2.2e-16
#alternative hypothesis: two-sided
ks.test(data_log$P_remove_eff_positive, 'pnorm')   # D = 0.98163, p-value < 2.2e-16
#alternative hypothesis: two-sided



# test homogeneity of variance of data
install.packages('tidyverse')
library(tidyverse)
library(ggplot2)

# scatterplots to visualize homogeneity
png("ggplot_P_conc.png", width=800, height=1000)
ggplot(data_log, aes(x = treatment, y = P_conc_inlet)) +
        geom_point() +
        ggtitle('Figure 1: treatment vs. P concentration')
dev.off()

png("ggplot_P_entrap.png", width=800, height=1000)
ggplot(data_log, aes(x = treatment, y = P_entrap_eff_biofilm)) +
  geom_point() +
  ggtitle('Figure 2: treatment vs. P entrapment efficiency')
dev.off()

png("ggplot_APA.png", width=800, height=1000)
ggplot(data_log, aes(x = treatment, y = APA)) +
  geom_point() +
  ggtitle('Figure 3: treatment vs. APA')
dev.off()

png("ggplot_Polyp.png", width=800, height=1000)
ggplot(data_log, aes(x = treatment, y = Poly_p)) +
  geom_point() +
  ggtitle('Figure 4: treatment vs. Polyphosphorus')
dev.off()

png("ggplot_P_biofilm.png", width=800, height=1000)
ggplot(data_log, aes(x = treatment, y = P_biofilm)) +
  geom_point() +
  ggtitle('Figure 5: treatment vs. P biofilm')
dev.off()

png("ggplot_P_balance.png", width=800, height=1000)
ggplot(data_log, aes(x = treatment, y = P_balance)) +
  geom_point() +
  ggtitle('Figure 6: treatment vs. P balance')
dev.off()

png("ggplot_P_remove.png", width=800, height=1000)
ggplot(data_log, aes(x = treatment, y = P_remove_eff)) +
  geom_point() +
  ggtitle('Figure 7: treatment vs. P removal efficiency')
dev.off()

# boxplots for visualisation
png("boxlot_P_conc.png", width=800, height=1000)
ggplot(data_log, aes(x = treatment, y = P_conc_inlet)) +
  geom_boxplot() +
  ggtitle('Figure 1: treatment vs. P concentration')
dev.off()

png("boxlot_P_entrap.png", width=800, height=1000)
ggplot(data_log, aes(x = treatment, y = P_entrap_eff_biofilm)) +
  geom_boxplot() +
  ggtitle('Figure 2: treatment vs. P entrapment efficiency')
dev.off()

png("boxlot_APA.png", width=800, height=1000)
ggplot(data_log, aes(x = treatment, y = APA)) +
  geom_boxplot() +
  ggtitle('Figure 3: treatment vs. APA')
dev.off()

png("boxlot_Poly_p.png", width=800, height=1000)
ggplot(data_log, aes(x = treatment, y = Poly_p)) +
  geom_boxplot() +
  ggtitle('Figure 4: treatment vs. Polyphosphate')
dev.off()

png("boxlot_P_biofilm.png", width=800, height=1000)
ggplot(data_log, aes(x = treatment, y = P_biofilm)) +
  geom_boxplot() +
  ggtitle('Figure 5: treatment vs. P biofilm')
dev.off()

png("boxlot_P_balance.png", width=800, height=1000)
ggplot(data_log, aes(x = treatment, y = P_balance)) +
  geom_boxplot() +
  ggtitle('Figure 6: treatment vs. P balance')
dev.off()

png("boxlot_P_remove.png", width=800, height=1000)
ggplot(data_log, aes(x = treatment, y = P_remove_eff)) +
  geom_boxplot() +
  ggtitle('Figure 7: treatment vs. P removal efficiency')
dev.off()


# statistical hypotheses: H0=all populations variances are equal, H1=at least two of the populations variances differ
# p-values less than 0.05 suggest variances are significantly different and homogeneity of variance assumption has been violated

# test homogeneity of variance using Bartlett's-test (sensitive to normal distribution)
bartlett.test(list(data_log$P_conc_inlet, data_log$P_entrap_eff_biofilm, data_log$APA, data_log$Poly_p, data_log$P_biofilm, data_log$P_balance, data_log$P_remove_eff))
# unsure if this test gives any usable result: Bartlett's K-squared = 120.15, df = 6, p-value < 2.2e-16

# Bartlett test for each variable vs treatment
bartlett_P_conc = bartlett.test(P_conc_inlet~treatment, data = data_log)
# Bartlett's K-squared = 8.5985, df = 3, p-value = 0.03513
bartlett_P_entrap_eff= bartlett.test(P_entrap_eff_biofilm~treatment, data = data_log)
# Bartlett's K-squared = 3.4548, df = 3, p-value = 0.3267
bartlett_APA = bartlett.test(APA~treatment, data = data_log)
# Bartlett's K-squared = 4.1981, df = 3, p-value = 0.2409
bartlett_Poly_p = bartlett.test(Poly_p~treatment, data = data_log)
# Bartlett's K-squared = 1.2799, df = 3, p-value = 0.7339
bartlett_P_biofilm = bartlett.test(P_biofilm~treatment, data = data_log)
# Bartlett's K-squared = 3.1465, df = 3, p-value = 0.3696
bartlett_P_balance = bartlett.test(P_balance~treatment, data = data_log)
# Bartlett's K-squared = 6.9661, df = 3, p-value = 0.07299
bartlett_P_remove_eff = bartlett.test(P_remove_eff~treatment, data = data_log)
# Bartlett's K-squared = 5.0865, df = 3, p-value = 0.1656

# test homogeneity of variance using Levene's test (most commonly used in literature, robust alternative, less sensitive to departures from normality)
library(car)

treatment_factor <- factor(data_log$treatment) # transform treatment column into factor variable (can contain string and integer as categorization of data, stores it as levels)
data_log2<- data.frame(treatment_factor, data_log[,2:8]) # put factor-treatment together with other log10 transformed data
print(is.factor(data_log2$treatment))

#levene test of variance centered around median

leveneTest(y = data_log2$P_conc_inlet, group = data_log2$treatment)
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
#group  3  1.7354 0.2465
#7               
leveneTest(y = data_log2$P_entrap_eff_biofilm, group = data_log2$treatment)
#     Df F value Pr(>F)
# group  3  0.4927 0.6985
# 7               
leveneTest(y = data_log2$APA, group = data_log2$treatment)
# Df F value Pr(>F)
#group  3  1.6387 0.2654
#7               
leveneTest(y = data_log2$Poly_p, group = data_log2$treatment)
#  Df F value Pr(>F)
#group  3  0.5381  0.671
#7           
leveneTest(y = data_log2$P_biofilm, group = data_log2$treatment)
#  Df F value Pr(>F)
#group  3  0.6338 0.6164
#7      
leveneTest(y = data_log2$P_balance, group = data_log2$treatment)
#       Df F value Pr(>F)
#group  3  1.3371 0.3371
#7               
leveneTest(y = data_log2$P_remove_eff, group = data_log2$treatment)
#     Df F value Pr(>F)
#group  3  0.9189 0.4795
#7   

# levene test of variance centered around mean

leveneTest(y = data_log2$P_conc_inlet, group = data_log2$treatment, center = mean)
# Levene's Test for Homogeneity of Variance (center = mean)
#        Df F value  Pr(>F)  
# group  3  3.9672 0.06066 .
# 7   
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
leveneTest(y = data_log2$P_entrap_eff_biofilm, group = data_log2$treatment, center = mean)
#     Df F value  Pr(>F)  
# group  3  3.4526 0.08023 .
# 7            
leveneTest(y = data_log2$APA, group = data_log2$treatment, center = mean)
#  Df F value  Pr(>F)   
# group  3  8.8458 0.00885 **
#  7               
leveneTest(y = data_log2$Poly_p, group = data_log2$treatment, center = mean)
#   Df F value Pr(>F)
# group  3  1.9017 0.2177
# 7          
leveneTest(y = data_log2$P_biofilm, group = data_log2$treatment, center = mean)
#    Df F value  Pr(>F)  
# group  3   4.862 0.03907 *
#  7    
leveneTest(y = data_log2$P_balance, group = data_log2$treatment, center = mean)
#      Df F value  Pr(>F)  
# group  3   4.862 0.03907 *
#  7    
leveneTest(y = data_log2$P_remove_eff, group = data_log2$treatment, center = mean)
#    Df F value  Pr(>F)  
# group  3  3.7002 0.06994 .
# 7


# test homogeneity of variance using Fligner-Killeen's test (a non-parametric test which is very robust against departures from normality)

fligner.test(P_conc_inlet ~ treatment, data = data_log2)
# Fligner-Killeen:med chi-squared = 2.9327, df = 3, p-value = 0.4021
fligner.test(P_entrap_eff_biofilm ~ treatment, data = data_log2)
# Fligner-Killeen:med chi-squared = 0.86583, df = 3, p-value = 0.8337
fligner.test(APA~ treatment, data = data_log2)
# Fligner-Killeen:med chi-squared = 3.1327, df = 3, p-value = 0.3716
fligner.test(Poly_p ~ treatment, data = data_log2)
# Fligner-Killeen:med chi-squared = 3.0339, df = 3, p-value = 0.3864
fligner.test(P_biofilm ~ treatment, data = data_log2)
# Fligner-Killeen:med chi-squared = 1.628, df = 3, p-value = 0.6531
fligner.test(P_balance ~ treatment, data = data_log2)
# Fligner-Killeen:med chi-squared = 2.1648, df = 3, p-value = 0.5389
fligner.test(P_remove_eff ~ treatment, data = data_log2)
# Fligner-Killeen:med chi-squared = 1.518, df = 3, p-value = 0.6781


# export data as csv
write.table(data_log2, "data_log.csv", sep = ",", dec = ".")



