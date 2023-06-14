# load data, display bar charts

getwd()
setwd("C:/RStudio/bachelorarbeit")
getwd()
library(dplyr) 
library("ggplot2")

#load data
data_log <- read.table("C:/RStudio/bachelorarbeit/data_log.csv", sep = ",", dec = ".", header = TRUE, na.strings = "")

treatment_factor <- factor(data_log$treatment) # transform treatment column into factor variable (can contain string and integer as categorization of data, stores it as levels)
data_log2<- data.frame(treatment_factor, data_log[,2:8]) # put factor-treatment together with other log10 transformed data


colors = c("dark green", "dark green", "dark green", "dark blue", "dark blue", "dark blue", "purple", "purple", "purple", "pink", "pink")
# bar charts
png("barplot_P_conc.png", width=800, height=1000)
barplot(data_log2$P_conc_inlet, names.arg=data_log2$treatment_factor, xlab="treatment", ylab="P concentration", col= colors, main="P concentration per treatment", border = "black")
dev.off()

png("barplot_P_entrap.png", width=800, height=1000)
barplot(data_log2$P_entrap_eff_biofil, names.arg=data_log2$treatment_factor, xlab="treatment", ylab="P entrapment efficiency", col= colors, main="P entrapment per treatment", border = "black")
dev.off()

png("barplot_APA.png", width=800, height=1000)
barplot(data_log2$APA, names.arg=data_log2$treatment_factor, xlab="treatment", ylab="APA", col= colors, main="APA per treatment", border = "black")
dev.off()

png("barplot_Poly_p.png", width=800, height=1000)
barplot(data_log2$Poly_p, names.arg=data_log2$treatment_factor, xlab="treatment", ylab="Polyphosphate", col= colors, main="Polyphosphate per treatment", border = "black")
dev.off()

png("barplot_P_biofilm.png", width=800, height=1000)
barplot(data_log2$P_biofilm, names.arg=data_log2$treatment_factor, xlab="treatment", ylab="P in biofilm", col= colors, main="P in biofilm per treatment", border = "black")
dev.off()

png("barplot_P_balance.png", width=800, height=1000)
barplot(data_log2$P_balance, names.arg=data_log2$treatment_factor, xlab="treatment", ylab="P balance", col= colors, main="P balance per treatment", border = "black")
dev.off()

png("barplot_P_remove.png", width=800, height=1000)
barplot(data_log2$P_remove_eff, names.arg=data_log2$treatment_factor, xlab="treatment", ylab="P removal efficiency", col= colors, main="P removal per treatment", border = "black")
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


data %>%
  group_by(treatment) %>%
  summarise_at(vars(P_conc), list(name = mean))
data %>%
  group_by(treatment) %>%
  summarise_at(vars(P_entrap_eff_biofilm), list(name = mean))
data %>%
  group_by(treatment) %>%
  summarise_at(vars(APA), list(name = mean))
data %>%
  group_by(treatment) %>%
  summarise_at(vars(Poly_p), list(name = mean))
data %>%
  group_by(treatment) %>%
  summarise_at(vars(P_biofilm), list(name = mean))
data %>%
  group_by(treatment) %>%
  summarise_at(vars(P_balance), list(name = mean))
data %>%
  group_by(treatment) %>%
  summarise_at(vars(P_remove_eff), list(name = mean))

# calculate group means for each treatment and variable and put them into new data frame
group_means = data.frame(treatment=c('P0','P1', 'P2', 'P3'),
                         P_conc=c(8.3, 48.7, 141.0, 411.0),
                         P_entrap_eff=c(1.49, 0.952, 2.09, 1.75),
                         P_balance=c(-0.89, 6.03, 11.7, 20.7),
                         P_remove_eff=c(-12.3, 14.0, 8.35, 4.28),
                         APA=c(0.0397, 0.0358, 0.0231, 0.0164),
                         Poly_p=c(6.34, 5.54, 5.90, 5.91),
                         P_biofilm=c(1.34, 2.56, 2.42, 3.29))



data_raw <- read.table("C:/RStudio/bachelorarbeit/first_data.csv", sep = ",", dec = ".", header = TRUE, na.strings = "")

#subset data
data_ohne_leerzeilen <- data_raw[1:11,]
data_ohne_charakter <- data_ohne_leerzeilen[,6:12]
#transform character in numeric values
data_num <- apply(data_ohne_charakter, 1:2, function(x){as.numeric(x)})
#add it together in data frame
data <- data.frame(data_ohne_leerzeilen[,4],data_num)

names(data) <-c("treatment", "P_conc", "P_balance", "P_remove_eff", "P_entrap_eff_biofilm", "APA", "Poly_p", "P_biofilm")


ggplot(data, aes(P_conc, treatment, fill = treatment)) +
  coord_flip() +
  stat_summary(geom = "bar", fun = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge")




col = c("#458B74", "#00688B", "#8968CD", "#FFBBFF")                         
# calculate new barplot containing all data                         
png("barplots_new.png", width=1300, height=800, res = 100)
par(mfrow=c(1,7))
barplot(group_means$P_conc, names.arg=group_means$treatment, xlab="treatment", ylab="P concentration (µg P/L)", col= col, main="P concentration", border = "black")
arrows (x0 = P_conc_barplot,
        y0 = summary_P_conc$mean + summary_P_conc$se,
        y1 = summary_P_conc$mean - summary_P_conc$se,
        angle = 90,
        code = 3,
        length = 0.1)
barplot(group_means$P_entrap_eff, names.arg=group_means$treatment, xlab="treatment", ylab="P entrapment efficiency (µg P/mg C)", col= col, main="P entrapment efficiency", border = "black")
arrows (x0 = P_entrap_barplot,
        y0 = summary_P_entrap$mean + summary_P_entrap$se,
        y1 = summary_P_entrap$mean - summary_P_entrap$se,
        angle = 90,
        code = 3,
        length = 0.1)
barplot(group_means$P_balance, names.arg=group_means$treatment, xlab="treatment", ylab="P balance (µg P/L)", col= col, main="P balance", border = "black")
arrows (x0 = P_balance_barplot,
        y0 = summary_P_balance$mean + summary_P_balance$se,
        y1 = summary_P_balance$mean - summary_P_balance$se,
        angle = 90,
        code = 3,
        length = 0.1)
barplot(group_means$P_remove_eff, names.arg=group_means$treatment, xlab="treatment", ylab="P removal efficiency (%)", col= col, main="P removal efficiency", border = "black")
arrows (x0 = P_remove_barplot,
        y0 = summary_P_remove$mean + summary_P_remove$se,
        y1 = summary_P_remove$mean - summary_P_remove$se,
        angle = 90,
        code = 3,
        length = 0.1)
barplot(group_means$APA, names.arg=group_means$treatment, xlab="treatment", ylab="APA (µmolMUF, cm-3. h-1). h", col= col, main="APA per treatment", border = "black")
arrows (x0 =APA_barplot,
        y0 = summary_APA$mean + summary_APA$se,
        y1 = summary_APA$mean - summary_APA$se,
        angle = 90,
        code = 3,
        length = 0.1)
barplot(group_means$Poly_p, names.arg=group_means$treatment, xlab="treatment", ylab="Polyphosphate (fluorescence)", col= col, main="Polyphosphate", border = "black")
arrows (x0 =Polyp_barplot,
        y0 = summary_Polyp$mean + summary_Polyp$se,
        y1 = summary_Polyp$mean - summary_Polyp$se,
        angle = 90,
        code = 3,
        length = 0.1)
barplot(group_means$P_biofilm, names.arg=group_means$treatment, xlab="treatment", ylab="P biofilm (µg P/cm³)", col= col, main="P biofilm", border = "black")
arrows (x0 =P_biofilm_barplot,
        y0 = summary_P_biofilm$mean + summary_P_biofilm$se,
        y1 = summary_P_biofilm$mean - summary_P_biofilm$se,
        angle = 90,
        code = 3,
        length = 0.1)
dev.off()



# calculate standard error for all variables
summary_P_conc <- aggregate(P_conc~treatment, data,    #create summary data containing mean and standard error
                          function(x) c(mean = mean(x),
                                        se = sd(x)/ sqrt(length(x))))
summary_P_conc <- data.frame(treatment = summary_P_conc[ , 1], summary_P_conc$P_conc)
P_conc_barplot <- barplot (summary_P_conc$mean ~ treatment,
                           summary_P_conc)

arrows (x0 = P_conc_barplot,
        y0 = summary_P_conc$mean + summary_P_conc$se,
        y1 = summary_P_conc$mean - summary_P_conc$se,
        angle = 90,
        code = 3,
        length = 0.1)

summary_P_entrap <- aggregate(P_entrap_eff_biofilm~treatment, data,    #create summary data containing mean and standard error
                            function(x) c(mean = mean(x),
                                          se = sd(x)/ sqrt(length(x))))
summary_P_entrap <- data.frame(treatment = summary_P_entrap[ , 1], summary_P_entrap$P_entrap_eff_biofilm)
P_entrap_barplot <- barplot (summary_P_entrap$mean ~ treatment,
                           summary_P_entrap)

arrows (x0 = P_entrap_barplot,
        y0 = summary_P_entrap$mean + summary_P_entrap$se,
        y1 = summary_P_entrap$mean - summary_P_entrap$se,
        angle = 90,
        code = 3,
        length = 0.1)

summary_P_balance <- aggregate(P_balance~treatment, data,    #create summary data containing mean and standard error
                              function(x) c(mean = mean(x),
                                            se = sd(x)/ sqrt(length(x))))
summary_P_balance<- data.frame(treatment = summary_P_balance[ , 1], summary_P_balance$P_balance)
P_balance_barplot <- barplot (summary_P_balance$mean ~ treatment,
                             summary_P_balance)

arrows (x0 = P_balance_barplot,
        y0 = summary_P_balance$mean + summary_P_balance$se,
        y1 = summary_P_balance$mean - summary_P_balance$se,
        angle = 90,
        code = 3,
        length = 0.1)

summary_P_remove <- aggregate(P_remove_eff~treatment, data,    #create summary data containing mean and standard error
                               function(x) c(mean = mean(x),
                                             se = sd(x)/ sqrt(length(x))))
summary_P_remove<- data.frame(treatment = summary_P_remove[ , 1], summary_P_remove$P_remove_eff)
P_remove_barplot <- barplot (summary_P_remove$mean ~ treatment,
                              summary_P_remove)

arrows (x0 = P_remove_barplot,
        y0 = summary_P_remove$mean + summary_P_remove$se,
        y1 = summary_P_remove$mean - summary_P_remove$se,
        angle = 90,
        code = 3,
        length = 0.1)

summary_APA <- aggregate(APA~treatment, data,    #create summary data containing mean and standard error
                              function(x) c(mean = mean(x),
                                            se = sd(x)/ sqrt(length(x))))
summary_APA<- data.frame(treatment = summary_APA[ , 1], summary_APA$APA)
APA_barplot <- barplot (summary_APA$mean ~ treatment,
                             summary_APA)

arrows (x0 =APA_barplot,
        y0 = summary_APA$mean + summary_APA$se,
        y1 = summary_APA$mean - summary_APA$se,
        angle = 90,
        code = 3,
        length = 0.1)

summary_Polyp <- aggregate(Poly_p~treatment, data,    #create summary data containing mean and standard error
                         function(x) c(mean = mean(x),
                                       se = sd(x)/ sqrt(length(x))))
summary_Polyp<- data.frame(treatment = summary_Polyp[ , 1], summary_Polyp$Poly_p)
Polyp_barplot <- barplot (summary_Polyp$mean ~ treatment,
                        summary_Polyp)

arrows (x0 =Polyp_barplot,
        y0 = summary_Polyp$mean + summary_Polyp$se,
        y1 = summary_Polyp$mean - summary_Polyp$se,
        angle = 90,
        code = 3,
        length = 0.1)

summary_P_biofilm <- aggregate(P_biofilm~treatment, data,    #create summary data containing mean and standard error
                           function(x) c(mean = mean(x),
                                         se = sd(x)/ sqrt(length(x))))
summary_P_biofilm<- data.frame(treatment = summary_P_biofilm[ , 1], summary_P_biofilm$P_biofilm)
P_biofilm_barplot <- barplot (summary_P_biofilm$mean ~ treatment,
                          summary_P_biofilm)

arrows (x0 =P_biofilm_barplot,
        y0 = summary_P_biofilm$mean + summary_P_biofilm$se,
        y1 = summary_P_biofilm$mean - summary_P_biofilm$se,
        angle = 90,
        code = 3,
        length = 0.1)