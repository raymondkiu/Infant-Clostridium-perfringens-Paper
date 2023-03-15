setwd("~/Desktop/pretermCP/Analysis-Rev1/Figure4/")
library(ggplot2)
library(tidyverse)
library(ggpubr)


# Set theme
library('cowplot')
theme_set(theme_cowplot())

data <- read.csv("plasmid_proportion_within_lineages.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)

# filter data
piechart1<- filter (data,Lineage == "VII") # do this for every lineage
head(piechart1)
# plot bar chart first then convert
PlasmidPalette <- c( "pCW3"="#e50534","pCP13"="#6600ff","Both"="Gold","No"="lightgrey")

pc1<- ggplot(piechart1, aes(x="", y=Value, fill=Plasmid))+
  scale_fill_manual(values=PlasmidPalette)+
  geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)+
  theme(axis.text.x=element_blank(),axis.line.x = element_blank(),axis.line.y=element_blank(),axis.ticks=element_blank(),legend.position="right")+
  ylab("") + xlab("")

pc1
