setwd("~/")
library(ggplot2)
library(tidyverse)
library(ggpubr)

data <- read.csv("toxinome_proportion_for_plotting.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)

# filter data
piechart1<- filter (data,Lineage == "VII") # Do this for every lineage
head(piechart1)
# plot bar chart first then convert
ToxinPalette <- c( "plc"="skyblue","pfoA"="red","ccp"="royalblue","colA"="slateblue","cpe"="yellow","etx"="black","alv"="grey","iap/ibp"="rosybrown","becA/B"="orange","cpb2"="plum")

pc1<- ggplot(piechart1, aes(x="", y=Perc, fill=Toxin))+
  scale_fill_manual(values=ToxinPalette)+
  geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)+
  theme(axis.text.x=element_blank(),axis.line.x = element_blank(),axis.line.y=element_blank(),axis.ticks=element_blank(),legend.position="right")+
  ylab("") + xlab("")

pc1
