setwd("~/Desktop/pretermCP/Analysis-Rev1/Figure4/")
library(ggplot2)
library(tidyverse)
library(ggpubr)


## NEC vs non-NEC bar chart
data <- read.csv("NECvsNonNEC.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)

# filter data
NECPalette <- c( "Yes"="purple","No"="grey90")

data$Lineage <- factor(data$Lineage, levels=c("Lineage V", "CPA-NEC", "Non-CPA-NEC"))

bc1 <- ggplot(data, aes(x=factor(Lineage), y=Perc, fill=factor(Plasmid)),position_stack(reverse = TRUE))+
  scale_fill_manual(values =NECPalette)+
  geom_bar(stat = "identity")+
  ylab("") + xlab("")+
  scale_y_continuous(breaks=c(0,25,50,75,100), limits=c(0,100)) + theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))
 

bc1
