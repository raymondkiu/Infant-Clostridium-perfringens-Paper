setwd("~/Desktop/pretermCP/Analysis-Rev1/Figure2/")
library(ggplot2)
library(tidyverse)
library(ggpubr)

data <- read.csv("virulence_count_across_lineage.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)

# Define a palette of colours for each genus that might occur in this plot.
# genusPalette define color - the only way
ToxinPalette <- c("plc"="skyblue","pfoA"="red","ccp"="royalblue","colA"="slateblue","cpe"="gold","etx"="black","iap/ibp"="rosybrown","becA/B"="orange","cpb2"="plum","alv"="grey")

# Make the plot.
p <-ggplot(data,
           aes(x= Toxin, y=Lineage,fill=Toxin))+ scale_y_discrete(limits = unique(rev(data$Lineage)))+ # reverse y-axis
  scale_x_discrete(limits = unique((data$Toxin)))+
  geom_point(aes(size = Perc,colour=Toxin),
             shape=21) + 
  scale_size(range = c(0, 8),
             guide = "legend",
             limits = c(0, 100),
             breaks = c(0, 25, 50, 75,100),
             labels = c("0%", "25%", "50%", "75%","100%")
  ) +
  scale_fill_manual(values= ToxinPalette, guide="none") +
  scale_color_manual(values=ToxinPalette)+
 theme(# Define the y axis title text size.
    axis.text.x= element_text(face="italic")) +
  labs(title="", x = "",  y="") + theme(legend.position="bottom")+
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1),legend.position = "bottom")

p
