setwd("~/")
library(ggplot2)
library(tidyverse)
library(ggpubr)

# colonisation plot
data <- read.csv("colonisation_gene_count_comparison.csv", header = TRUE, stringsAsFactors = TRUE)
data
p <- ggplot(data, aes(x=Lineage, y=Colonisation, fill=Lineage)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = 0.1, notch=FALSE,varwidth = FALSE, size=0.2) +
  scale_y_continuous(breaks = c(0,2,4,6,8,10,12,14,16),limits=c(0, 15)) + geom_jitter(width = 0.05, alpha=0.1) + scale_fill_manual(values=c("I"="#ba1114","III"="#ebda26","V"="#122cc4","VI"="#00d9ff","VII"="#4b0c71"))
p
