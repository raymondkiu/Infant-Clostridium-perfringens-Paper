setwd("~/Desktop/pretermCP/Analysis-Rev1/InvivoHistology/")
library(tidyverse)

# Input
# Group	Category	Score
# Control	Inflammatory infiltration	0.5
# Control	Inflammatory infiltration	1
# Control	Inflammatory infiltration	0.5
# Control	Inflammatory infiltration	1
# Control	Inflammatory infiltration	0
# Control	Epithelial hyperplasia and goblet cell loss	0.5
# Control	Epithelial hyperplasia and goblet cell loss	0
# Control	Epithelial hyperplasia and goblet cell loss	0
# Control	Epithelial hyperplasia and goblet cell loss	1
# Control	Epithelial hyperplasia and goblet cell loss	0
# Control	Mucosal architecture	0.5
# Control	Mucosal architecture	0.5
# Control	Mucosal architecture	0.5
# Control	Mucosal architecture	2
# Control	Mucosal architecture	0.5
# ABX	Inflammatory infiltration	1.5
# ABX	Inflammatory infiltration	0.5
# ABX	Inflammatory infiltration	1.5

data <- read.csv("R-histoScore.csv", header = TRUE, stringsAsFactors = TRUE)
data

# Use the theme cowplot
library('cowplot')
theme_set(theme_cowplot())

library(Rmisc)
data<- summarySE(data, measurevar="Score", groupvars=c("Group","pfoA")) # Group first then Isolate
data

# reorder
data$Group <- factor(data$Group, levels = c("Control", "ABX","IQ146","IQ129","LH115","IQ147","IQ133","LH043"))
data$pfoA<- factor(data$pfoA,levels=c("Control","ABX","pfoA+","pfoA-"))
#data$Category <- factor(data$Category,levels=c("Inflammatory infiltration", "Epithelial hyperplasia and goblet cell loss","Mucosal architecture"))

# Build a stacked bar plots with mean pathological scores of each category
p <- 
  ggplot(data,aes(x=pfoA, y=Score,fill=pfoA)) +
  geom_bar(stat='identity',position='stack') +
  scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14, limits=c(0,15)))
p+ scale_fill_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","pfoA+"="Grey","pfoA-"="Grey") +
#p+  scale_fill_manual(values=c("Purple4", "darkorchid1", "grey")) + 
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))

# Build a boxplot 
# Load data (different from above)
data <- read.csv("R-histoScore.csv", header = TRUE, stringsAsFactors = TRUE)
data

# Reorder
data$Group <- factor(data$Group, levels = c("Control", "ABX","IQ146","IQ129","LH115","IQ147","IQ133","LH043"))
data$pfoA <- factor(data$pfoA,levels=c("Control","ABX","pfoA+","pfoA-"))
# Do not use summary stats for geom_boxplot because geom_boxplot will calculate the stats, do not use error bar, it will generate automatically

b <- 
  ggplot(data,aes(x=pfoA, y=Score, fill=pfoA)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = NA, notch=FALSE,varwidth = FALSE, size=0.2) +
  #scale_color_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","CP"="red"))+
  scale_y_continuous(breaks=c(0,2,4,6,8,10,12), limits=c(-0.5,12.5))
b 
b+scale_fill_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","pfoA+"="red2","pfoA-"="Grey"))+ 
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))
#b +scale_fill_manual(values=c("lightgreen","mediumpurple1","red")) 


# Stats
Data <- read.csv("R-histoScore2.csv", header = TRUE, stringsAsFactors = TRUE)
head(Data)

# Any difference in stats?
shapiro.test(Data$Score)
# if no difference to normal distribution, then do aov (anova) then tukey post hoc
res.aov <- aov(Score ~ Group, data = Data)
summary(res.aov)
TukeyHSD(res.aov)


library(FSA)
Summarize(Score~ Group, data = Data)
res.kruskal <- kruskal.test(Score ~ Group, data=Data)
res.kruskal

# Test which group is statistically different, suitable for unequal number of observations
dunnTest(Data$Score, Data$Group,method="bonferroni")
library(rstatix)
dunn_test(Data, Score ~ Group, p.adjust.method="BH")
######################
# Tetracycline group #
######################
# Input
# Group	Score
# Control	3
# Control	1.5
# Control	2.5
# Control	1
# Control	4
# Group A	6.5
# Group A	5.5
# Group A	6
# Group A	6.5
# Group A	6.5

# Build a boxplot 
# Load data 
data <- read.csv("Tet2.csv", header = TRUE, stringsAsFactors = TRUE)
data

# Reorder
data$Group <- factor(data$Group, levels = c("Control", "Group A","Group B"))
# Do not use summary stats for geom_boxplot because geom_boxplot will calculate the stats, do not use error bar, it will generate automatically

b <- 
  ggplot(data,aes(x=Group, y=Score, fill=Group)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = NA, notch=FALSE,varwidth = TRUE, size=0.2) +
  #scale_color_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","CP"="red"))+
  scale_y_continuous(breaks=c(0,2,4,6,8,10,12), limits=c(-0.5,12))
b 
b +scale_fill_manual(values=c("lightgreen","red","orange")) + theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))

# Stats
Data <- read.csv("Tet2.csv", header = TRUE, stringsAsFactors = TRUE)
Data

library(FSA)
Summarize(Score~ Group, data = Data)

res.kruskal <- kruskal.test(Score ~ Group, data=Data)
res.kruskal
# Test which group is statistically different, suitable for unequal number of observations
dunnTest(Data$Score, Data$Group,method="none")
