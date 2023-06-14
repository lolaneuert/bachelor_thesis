# load data, calculate pca chart

setwd("C:/RStudio/bachelorarbeit")

#load data
data_log <- read.table("C:/RStudio/bachelorarbeit/data_log.csv", sep = ",", dec = ".", header = TRUE, na.strings = "")
treatment_factor <- factor(data_log$treatment) # transform treatment column into factor variable (can contain string and integer as categorization of data, stores it as levels)
data_log2<- data.frame(treatment_factor, data_log[,2:8]) # put factor-treatment together with other log10 transformed data

#install and load the packages that we need for the analysis
install.packages("stats")
install.packages("factoextra")
library(stats)
library(factoextra)
library(tidyverse)


#PCA ANALYSIS (script chunk from Nuria)
pdf("PCA_from_Nuria.pdf",
    width = 8, height = 9,
    paper = "A4")
res_pca <- prcomp(data_log2[,2:8], scale = TRUE)   # calculates principal components of dataset: x must be numeric, therefore not treatment factor included
fviz_eig(res_pca)
fviz_pca_var(res_pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#009ACD","#CD6090","#EE9A00" ),
             repel = TRUE)    # Avoid text overlapping
dev.off()

summary(res_pca)
print(res_pca)

pdf("PCA.pdf",
    width = 8, height = 9,
    paper = "A4")
res_pca <- prcomp(data_log2[,2:8], scale = TRUE)   # calculates principal components of dataset: x must be numeric, therefore not treatment factor included
fviz_eig(res_pca)
fviz_pca_var(res_pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#009ACD","#FF34B3","#EEAD0E" ),
             repel = TRUE)    # Avoid text overlapping
dev.off()

# biplot
pdf("PCA_biplot.pdf", width = 8, height = 9, paper = "A4")
fviz_pca_biplot(res_pca,
                col.var = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                repel = TRUE)

dev.off()

###

res_pca$rotation <- -1*res_pca$rotation    # reverse signs (eigenvectors in R point in negative direction by default)
res_pca$rotation                            

res_pca$x <- -1*res_pca$x                  # reverse sign of scores
head(res_pca$x)

# biplot
biplot(res_pca, scale = 0)   

res_pca$sdev^2 / sum(res_pca$sdev^2)       # calculate total variance explained by each principal component
# [1] 0.504767155 0.274021771 0.126188213 0.054232933
# [5] 0.026375009 0.009768745 0.004646174
# therefore: 1st PC: 50.5%, 2nd PC: 27.4%, 3rd PC: 12.6%, 4th PC: 5.4%, 5th PC: 2.6%, 6th PC: 0.9%, 7th PC: 0.4%


var_explained = res_pca$sdev^2 / sum(res_pca$sdev^2)   # calculate total variance explained by each principal component

# scree plot
qplot(c(2:8), var_explained) +
geom_line() +
  xlab("Principal Component") +
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0,1)


res_pca$sdev^2 / sum(res_pca$sdev^2)
