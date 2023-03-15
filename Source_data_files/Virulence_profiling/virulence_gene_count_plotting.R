setwd("~/")
library(ggplot2)
library(tidyverse)
library(ggpubr)

data <- read.csv("virulence_gene_count_comparison", header = TRUE, stringsAsFactors = TRUE)
data

# Basic box plot theme
library('cowplot')
theme_set(theme_cowplot())

p <- 
 data %>%
  ggplot(aes(x=Lineage, y=Virulence_gene_count, fill=Lineage)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = 0.1, notch=FALSE,varwidth = FALSE, size=0.2) +
  scale_y_continuous(limits=c(0, 25)) + geom_jitter(width = 0.05, alpha=0.1)
p
p+ scale_fill_manual(values=c("I"="#ba1114","III"="#ebda26","V"="#122cc4","VI"="#00d9ff","VII"="#4b0c71"))

