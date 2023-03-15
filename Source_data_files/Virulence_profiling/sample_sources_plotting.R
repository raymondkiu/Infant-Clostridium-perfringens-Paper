setwd("~/Desktop/pretermCP/Analysis-Rev1/Figure2/")
library(ggplot2)
library(tidyverse)
library(ggpubr)

# pie chart -> child/neonate/sample source %

data <- read.csv("childneonate.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)

# filter data
piechart1<- filter (data,Lineage == "I") # do this for every lineage
head(piechart1)
# plot bar chart first then convert
ChildPalette <- c( "Child"="#86eba3","Neonate"="#eb021a")

pc1<- ggplot(piechart1, aes(x="", y=Perc, fill=Sample))+
  scale_fill_manual(values=ChildPalette)+
  geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)+
  theme(axis.text.x=element_blank(),axis.line.x = element_blank(),axis.line.y=element_blank(),axis.ticks=element_blank(),legend.position="right")+
  ylab("") + xlab("")

pc1
