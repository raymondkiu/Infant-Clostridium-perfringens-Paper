library(ggfortify)
library(cluster)
library(gplots)
library(ggplot2)
library(RColorBrewer)

setwd("")
data <- read.csv(file = "file.csv", check.names=TRUE)
data

head(data[1])
df <- data[c(3:ncol(data))] # extract data from column 3 and onwards
head(df)

data$Group<- factor(data$Group, levels = c("Bell Stage II/III", "Bell Stage I", "Non-NEC"))
data

# apply theme
library('cowplot')
theme_set(theme_cowplot())

# prcomp (pca estimation) data
pca <- prcomp(df)
# Draw plot
autoplot(pca,data=data,colour='Group', label=FALSE, frame=FALSE,frame.colour = 'Group',loadings=TRUE,loadings.label=TRUE,loadings.label.repel=TRUE,size=3,loadings.label.size=3.5,loadings.label.colour="black")+
  scale_color_manual(values=c("red","gold","darkturquoise")) +
  theme(legend.position="right", legend.direction="vertical", legend.title = element_blank())

dev.off()
