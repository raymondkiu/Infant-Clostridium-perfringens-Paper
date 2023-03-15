setwd("~/")
library(tidyverse)

# Build a boxplot 
# Load data (different from above)
data <- read.csv("histo_score_total_data.csv", header = TRUE, stringsAsFactors = TRUE)
data

# Reorder
data$pfoA <- factor(data$pfoA,levels=c("Control","ABX","pfoA+","pfoA-"))

b <- 
  ggplot(data,aes(x=pfoA, y=Score, fill=pfoA)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = NA, notch=FALSE,varwidth = FALSE, size=0.2) +
  #scale_color_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","CP"="red"))+
  scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14), limits=c(-0.5,14))
b 

b+scale_fill_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","pfoA+"="red2","pfoA-"="Grey"))+ 
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))


# Stats
Data <- read.csv("histo_score_total_data.csv", header = TRUE, stringsAsFactors = TRUE)
head(Data)

library(FSA)
Summarize(Score~ Group, data = Data)
res.kruskal <- kruskal.test(Score ~ Group, data=Data)
res.kruskal

library(rstatix)
dunn_test(Data, Score ~ Group, p.adjust.method="BH")
