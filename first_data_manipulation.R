# basic data manipulation for BA: transform excel sheet, so that I can use it in R, log10 transform all data, 
# calculate 5 Point statistics: Min, Max, Median, Mean, NA's, 1st & 3rd Quantil, calculate standarddeviation and variance, simply plot data

getwd()
setwd("C:/RStudio/bachelorarbeit")
getwd()

install.packages("dplyr")              # to install
library(dplyr)   


#load data
data_raw_BA <- read.table("C:/RStudio/bachelorarbeit/first_data.csv", sep = ",", dec = ".", header = TRUE, na.strings = "")

#subset data
data_ohne_leerzeilen <- data_raw_BA[1:11,]
data_ohne_charakter <- data_ohne_leerzeilen[,6:12]
#transform character in numeric values
data_num <- apply(data_ohne_charakter, 1:2, function(x){as.numeric(x)})
#add it together in data frame
data_sub <- data.frame(data_ohne_leerzeilen[,4],data_num)

#adjust column names
names(data_sub) <-c("treatment", "P_conc_inlet", "P_balance", "P_remove_eff", "P_entrap_eff_biofilm", "APA", "Poly_p", "P_biofilm")


#for each variable with negative values, 150 was added into a new column before the log10 transformation
data_sub$P_balance_positive <- data_sub[,3] + 150
data_sub$P_remove_eff_positive <- data_sub[,4] + 150

#subset only positive variables
data_pos <- data_sub[, -c(3,4)]

# log10 transformation
log_transf <- lapply(data_pos[,c(2:8)], function(x) log10(x))
data_log <- data.frame(data_ohne_leerzeilen[,4],log_transf)
# produce new data frame that contains the values for each column in data_pos, but has log10 transformed data,exclude treatment column in calculations and add them back together in a dataframe afterwards

png("histogramm_P_conc_raw_log.png")
par(mfrow=c(1,2))
hist(data_pos$P_conc_inlet, col='light blue', main='raw data') #histogramm of the raw data
hist(data_log$P_conc_inlet, col='dark blue', main='log10 transformed data') #historgramm of the log10 transformed data
mtext("Histogramm P concentration raw vs log10", side=3, line=-1, outer= TRUE)
dev.off()

# first statistics
# 5 Point statistics: Min, Max, Median, Mean, NA's, 1st & 3rd Quantil

summary(data_sub$P_conc_inlet)     
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  8.00   17.60   69.68  128.83  150.15  444.45 

summary(data_sub$P_balance)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -2.020   1.745   5.650   8.358  13.845  23.960 

summary(data_sub$P_remove_eff)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-27.340   2.365   5.310   3.531  10.820  21.440 

summary(data_sub$P_entrap_eff_biofilm)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.4909  0.7652  1.2667  1.5546  2.2335  3.3548 

summary(data_sub$APA)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.01065 0.02292 0.02468 0.02985 0.04040 0.04460 

summary(data_sub$Poly_p)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 5.119   5.556   5.775   5.926   6.189   7.357 

summary(data_sub$P_biofilm)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  1.250   1.562   2.400   2.322   2.844   3.758 

# Standarddeviation
sd(data_sub$P_conc_inlet, na.rm = T)        # 150.5709
sd(data_sub$P_balance, na.rm = T)           # 9.355642
sd(data_sub$P_remove_eff, na.rm = T)        # 14.57076
sd(data_sub$P_entrap_eff_biofilm, na.rm = T)# 1.034822
sd(data_sub$APA, na.rm = T)                 # 0.01117497
sd(data_sub$Poly_p, na.rm = T)              # 0.6649478
sd(data_sub$P_biofilm, na.rm = T)           # 0.8073582

# Variance
var(data_sub$P_conc_inlet, na.rm = T)       # 22671.59
var(data_sub$P_balance, na.rm = T)          # 87.52804
var(data_sub$P_remove_eff, na.rm = T)       # 212.3071
var(data_sub$P_entrap_eff_biofilm, na.rm = T)# 1.070856
var(data_sub$APA, na.rm = T)                #  0.00012488
var(data_sub$Poly_p, na.rm = T)             #  0.4421821
var(data_sub$P_biofilm, na.rm = T)          # 0.6518273  


#simple plots of the variables (plotted vs index )
png("simple_index_plots.png")
par(mfrow = c(4,2))
plot(data_sub$P_conc_inlet)
plot(data_sub$P_balance)
plot(data_sub$P_remove_eff)
plot(data_sub$P_entrap_eff_biofilm)
plot(data_sub$APA)
plot(data_sub$Poly_p)
plot(data_sub$P_biofilm)
mtext("simple plots", side=3, line=-2, outer=TRUE)
dev.off()