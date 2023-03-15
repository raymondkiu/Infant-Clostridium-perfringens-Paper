setwd("~/")
library(ggplot2)
library(tidyverse)

data <- read.csv("snp-distance-file.csv", header = TRUE, stringsAsFactors = TRUE)
data
data$Lineage <- factor(data$Lineage, labels = c("BL","I", "II","III","IV","V","VI","VII","VIII"))
data

# Basic box plot theme
library('cowplot')
theme_set(theme_cowplot())

p <- 
  data %>%
  mutate(Lineage = fct_relevel(Lineage, "I", "II","III","IV","V","VI","VII","VIII","BL")) %>% 
  ggplot(aes(x=Lineage, y=SNP, fill=Lineage)) + 
  geom_boxplot(outlier.colour = "white", outlier.shape = NA, outlier.alpha = 0, notch=FALSE,varwidth = FALSE, size=0.2) +
  scale_y_continuous(limits=c(-5,2500)) + geom_jitter(width = 0.05, alpha=0)
p

p+scale_fill_manual(values=c("#BA1114","#F79EA4","#EBDA26","#701501","#122CC4","#00D9FF","#4B0C71","#DAAFF3", "Grey"))
