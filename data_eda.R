# load data, formulate EDA function, calculate EDA plots via function for each variable

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


#load data
data_log <- read.table("C:/RStudio/bachelorarbeit/data_log.csv", sep = ",", dec = ".", header = TRUE, na.strings = "")

EDA_plots <- function(x){
  par(mfrow = c(2,2))
  hist(x)
  boxplot(x)
  qqnorm(x)
  qqline(x)
  plot(density(x[!is.na(x)]))
  abline(v = mean(x, na.rm = T), col = "red")
  abline(v = median(x, na.rm = T), col = "green")
  abline(v = sd(x, na.rm = T), col = "blue")
}

png("EDA_plots_P_conc.png", width = 800, height = 1000)
EDA_plots(data$P_conc_inlet)
mtext("P conc plots", side=3, line=-2, outer=TRUE)
dev.off()

png("EDA_plots_P_entrapment_eff.png", width = 800, height = 1000)
EDA_plots(data$P_entrap_eff_biofilm)
mtext("P entrapment eff: plots", side=3, line=-2, outer=TRUE)
dev.off()

png("EDA_plots_APA.png", width = 800, height = 1000)
EDA_plots(data$APA)
mtext("APA: plots", side=3, line=-2, outer=TRUE)
dev.off()

png("EDA_plots_Polyphosphate.png", width = 800, height = 1000)
EDA_plots(data$Poly_p)
mtext("Polyphosphate: plots", side=3, line=-2, outer=TRUE)
dev.off()

png("EDA_plots_P_biofilm.png", width = 800, height = 1000)
EDA_plots(data$P_biofilm)
mtext("P biofilm: plots", side=3, line=-2, outer=TRUE)
dev.off()

png("EDA_plots_P_balance.png", width = 800, height = 1000)
EDA_plots(data$P_balance)
mtext("P balance: plots", side=3, line=-2, outer=TRUE)
dev.off()

png("EDA_plots_P_removal_eff.png", width = 800, height = 1000)
EDA_plots(data$P_remove_eff)
mtext("P removal eff: plots", side=3, line=-2, outer=TRUE)
dev.off()


data_raw <- read.table("C:/RStudio/bachelorarbeit/first_data.csv", sep = ",", dec = ".", header = TRUE, na.strings = "")

#subset data
data_ohne_leerzeilen <- data_raw[1:11,]
data_ohne_charakter <- data_ohne_leerzeilen[,6:12]
#transform character in numeric values
data_num <- apply(data_ohne_charakter, 1:2, function(x){as.numeric(x)})
#add it together in data frame
data <- data.frame(data_ohne_leerzeilen[,4],data_num)

names(data) <-c("treatment", "P_conc_inlet", "P_balance", "P_remove_eff", "P_entrap_eff_biofilm", "APA", "Poly_p", "P_biofilm")

png("EDA_plots_P_con.png", width = 800, height = 1000)
EDA_plots(data$P_conc_inlet)
dev.off()

png("EDA_plots_P_entrapment_eff.png", width = 800, height = 1000)
EDA_plots(data$P_entrap_eff_biofilm)
dev.off()

png("EDA_plots_APA.png", width = 800, height = 1000)
EDA_plots(data$APA)
dev.off()

png("EDA_plots_Polyphosphate.png", width = 800, height = 1000)
EDA_plots(data$Poly_p)
dev.off()

png("EDA_plots_P_biofilm.png", width = 800, height = 1000)
EDA_plots(data$P_biofilm)
dev.off()

png("EDA_plots_P_balance.png", width = 800, height = 1000)
EDA_plots(data$P_balance)
dev.off()

png("EDA_plots_P_removal_eff.png", width = 800, height = 1000)
EDA_plots(data$P_remove_eff)
dev.off()