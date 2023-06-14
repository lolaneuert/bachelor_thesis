#

#set working directory
getwd()
setwd("C:/RStudio/bachelorarbeit")
getwd()

# install packages
library(dplyr)   
library(stats)
library(factoextra)

data_raw <- read.table("C:/RStudio/bachelorarbeit/first_data.csv", sep = ",", dec = ".", header = TRUE, na.strings = "")

#subset data
data_ohne_leerzeilen <- data_raw[1:11,]
data_ohne_charakter <- data_ohne_leerzeilen[,6:12]
#transform character in numeric values
data_num <- apply(data_ohne_charakter, 1:2, function(x){as.numeric(x)})
#add it together in data frame
data <- data.frame(data_ohne_leerzeilen[,4],data_num)

names(data) <-c("treatment", "P_conc_inlet", "P_balance", "P_remove_eff", "P_entrap_eff_biofilm", "APA", "Poly_p", "P_biofilm")

distribution_histogram <- function(x){
  par(mfrow=c(1,2))
  hist(data_pos$P_conc_inlet, col='light blue', main='raw')
  hist(data_log$P_conc_inlet, col='dark blue', main='log')
  mtext("Histogram", side=3, line=-1, outer= TRUE)
}

library(tidyverse)
library(ggplot2)

# scatterplots to visualize homogeneity
png("ggplot_P_conc.png", width=800, height=1000)
ggplot(data, aes(x = treatment, y = P_conc_inlet)) +
  geom_point() +
  ggtitle('scatterplot: treatment vs. P concentration')
dev.off()

png("ggplot_P_entrap.png", width=800, height=1000)
ggplot(data, aes(x = treatment, y = P_entrap_eff_biofilm)) +
  geom_point() +
  ggtitle('scatterplot: treatment vs. P entrapment efficiency')
dev.off()

png("ggplot_APA.png", width=800, height=1000)
ggplot(data, aes(x = treatment, y = APA)) +
  geom_point() +
  ggtitle('scatterplot: treatment vs. APA')
dev.off()

png("ggplot_Polyp.png", width=800, height=1000)
ggplot(data, aes(x = treatment, y = Poly_p)) +
  geom_point() +
  ggtitle('scatterplot: treatment vs. Polyphosphorus')
dev.off()

png("ggplot_P_biofilm.png", width=800, height=1000)
ggplot(data, aes(x = treatment, y = P_biofilm)) +
  geom_point() +
  ggtitle('scatterplot: treatment vs. P biofilm')
dev.off()

png("ggplot_P_balance.png", width=800, height=1000)
ggplot(data, aes(x = treatment, y = P_balance)) +
  geom_point() +
  ggtitle('scatterplot: treatment vs. P balance')
dev.off()

png("ggplot_P_remove.png", width=800, height=1000)
ggplot(data, aes(x = treatment, y = P_remove_eff)) +
  geom_point() +
  ggtitle('scatterplot: treatment vs. P removal efficiency')
dev.off()

# boxplots for visualisation
png("boxplot_P_conc.png", width=800, height=1000)
ggplot(data, aes(x = treatment, y = P_conc_inlet)) +
  geom_boxplot() +
  ggtitle('boxplot: treatment vs. P concentration')
dev.off()

png("boxplot_P_entrap.png", width=800, height=1000)
ggplot(data, aes(x = treatment, y = P_entrap_eff_biofilm)) +
  geom_boxplot() +
  ggtitle('boxplot: treatment vs. P entrapment efficiency')
dev.off()

png("boxplot_APA.png", width=800, height=1000)
ggplot(data, aes(x = treatment, y = APA)) +
  geom_boxplot() +
  ggtitle('boxplot: treatment vs. APA')
dev.off()

png("boxplot_Poly_p.png", width=800, height=1000)
ggplot(data, aes(x = treatment, y = Poly_p)) +
  geom_boxplot() +
  ggtitle('boxplot: treatment vs. Polyphosphate')
dev.off()

png("boxplot_P_biofilm.png", width=800, height=1000)
ggplot(data, aes(x = treatment, y = P_biofilm)) +
  geom_boxplot() +
  ggtitle('boxplot: treatment vs. P biofilm')
dev.off()

png("boxplot_P_balance.png", width=800, height=1000)
ggplot(data, aes(x = treatment, y = P_balance)) +
  geom_boxplot() +
  ggtitle('boxplot: treatment vs. P balance')
dev.off()

png("boxplot_P_remove.png", width=800, height=1000)
ggplot(data, aes(x = treatment, y = P_remove_eff)) +
  geom_boxplot() +
  ggtitle('boxplot: treatment vs. P removal efficiency')
dev.off()

