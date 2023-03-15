setwd("~/")
library(tidyverse)

# Data format:
# Group	Individual	Sample	Mucispirillum	Bacteroides	Odoribacter	Prevotella	Bilophila	Desulfovibrio	Staphylococcus	Enterococcus	Lactobacillus
# ABX	Day 1	ABX1M1	0	0.774499136	0	0	0	0	0	90.03392434	9.191576522
# ABX	Day 1	ABX1M2	0	0.016104788	0	0	0	0	0	97.02061413	2.963281082

# Barplot for 16S visualisation
data <- read.csv("taxa_data_plotting.csv", header = TRUE, stringsAsFactors = TRUE)
data

# melt data
plotDat <- reshape::melt(data, id=c("Group","Day"))
head(plotDat)
# Basic box plot
library('cowplot')
theme_set(theme_cowplot())

library(Rmisc)
plotDat<- summarySE(plotDat, measurevar="value", groupvars=c("Group","Day","variable")) # Group first then Isolate
plotDat

# reorder
#plotDat$Group <- factor(plotDat$Group, levels = c("Control", "ABX","IQ146","IQ129","LH115","IQ147","IQ133","LH043"))
plotDat$Group <- factor(plotDat$Group, levels = c("Control", "ABX","PFO+","PFO-"))
p <- 
  ggplot(plotDat,aes(x=Day, y=value,fill=variable)) +
  geom_bar(stat='identity',position='stack') +
  scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(-5,104))
p

# Genuspalette define color - the only way
genusPalette <- c( Mucispirillum="tan3",
                  Bacteroides="lightpink",
                  Bacillus="lightyellow",
                  Prevotella="hotpink",
                  Jeotgalicoccus="dodgerblue",
                  Desulfovibrio="navy",
                  Staphylococcus="orange",
                  Enterococcus="gold",
                  Lactobacillus="slateblue3",
                  Streptococcus="purple",
                  Candidatus.Arthromitus="lightblue",
                  Clostridium="red1",
                  Blautia="darkgreen",
                  Coprococcus="brown4",
                  Dorea="mediumpurple1",
                  Dehalobacterium="palegreen",
                  Anaerotruncus="sienna1",
                  Oscillospira="royalblue",
                  Ruminococcus="yellowgreen",
                  Allobaculum="tomato1",
                  Coprobacillus="darkorange",
                  Anaeroplasma="cyan")

p + scale_fill_manual(values = genusPalette) +
facet_grid( ~ Group, scales="free") + 
theme(
  axis.text.x = element_text(angle = 45, vjust=1, hjust=1),
     axis.title.x=element_blank())
