setwd("~/")
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(Rmisc)

data <- read.csv("histo_score_features_data.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)

# set theme - this is a nice one - minimalist
library('cowplot')
theme_set(theme_cowplot())

# Prep stats for error bar: Infiltration feature only
data_infiltration <- data %>% mutate(Group = fct_relevel(Group, "Control","ABX","pfoA+","pfoA-"))# reorder first
#data_infiltration <- summarySE(data, measurevar="Infiltration", groupvars=c("Group"))


# Box plot: inflammatory cell infiltrate
p <- 
  ggplot(data_infiltration,aes(x=Group, y=Infiltration, fill=Group)) +
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = NA, notch=FALSE,varwidth = FALSE, size=0.2, width=0.8) +
  scale_y_continuous(breaks=c(0,1,2,3,4), limits=c(0,4)) +
#  geom_errorbar(aes(ymin=Infiltration-se, ymax=Infiltration+se),
#                width=.3,                    # Width of the error bars
#                position=position_dodge(.9))+
  scale_fill_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","pfoA+"="Red2","pfoA-"="Grey"))+
  ylab("")+xlab("")+theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))

p + theme(axis.text.x = element_blank(),axis.ticks.x=element_blank()) + ylab("")+
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))

# Box plot: hyperplasia, epithelial changes
data <- read.csv("R-histoInvididualScores.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)
data_hyperplasia <- data %>% mutate(Group = fct_relevel(Group, "Control","ABX","pfoA+","pfoA-"))# reorder first

p <- 
  ggplot(data_hyperplasia,aes(x=Group, y=Hyperplasia, fill=Group)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = NA, notch=FALSE,varwidth = FALSE, size=0.2, width=0.8) +
  scale_y_continuous(breaks=c(0,1,2,3,4,5), limits=c(0,5)) +
 
  scale_fill_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","pfoA+"="Red2","pfoA-"="Grey"))+
  ylab("")+xlab("")+theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))

p + theme(axis.text.x = element_blank(),axis.ticks.x=element_blank()) + ylab("")+
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))

# Box plot: mucosal architecture
data <- read.csv("R-histoInvididualScores.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)
data_architecture <- data %>% mutate(Group = fct_relevel(Group, "Control","ABX","pfoA+","pfoA-"))# reorder first

p <- 
  ggplot(data_architecture,aes(x=Group, y=Architecture, fill=Group)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = NA, notch=FALSE,varwidth = FALSE, size=0.2, width=0.8) +
  scale_y_continuous(breaks=c(0,1,2,3,4,5), limits=c(0,5)) +
  scale_fill_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","pfoA+"="Red2","pfoA-"="Grey"))+
  ylab("")+xlab("")+theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))

p + theme(axis.text.x = element_blank(),axis.ticks.x=element_blank()) + ylab("")+
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))

# Total scores
data <- read.csv("histo_score_features_data.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)
data <- data %>% mutate(Group = fct_relevel(Group, "Control","ABX","pfoA+","pfoA-"))# reorder first

p <- 
  ggplot(data,aes(x=Group, y=Total, fill=Group)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = NA, notch=FALSE,varwidth = FALSE, size=0.2, width=0.8) +
  scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14), limits=c(0,14)) +
  scale_fill_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","pfoA+"="Red2","pfoA-"="Grey"))+
  ylab("")+xlab("")+theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))

p + theme(axis.text.x = element_blank(),axis.ticks.x=element_blank()) + ylab("")+
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))


## Stats
data <- read.csv("", header = TRUE, stringsAsFactors = TRUE)
head(data)
shapiro.test(data$Infiltration) # surely different from normal distribution so it is non-parametric
shapiro.test(data$Hyperplasia)
shapiro.test(data$Architecture)

# Run statistical analysis
kruskal.test(Infiltration ~ Group, data=data) # P<0.0001
kruskal.test(Hyperplasia ~ Group, data=data) # P<0.0001
kruskal.test(Architecture ~ Group, data=data) # P<0.0001

# Test which group is statistically different (if overall KW test P<0.05), suitable for unequal number of observations
library(rstatix)
dunn_test(data, Infiltration ~ Group, p.adjust.method = "BH")
dunn_test(data, Hyperplasia ~ Group, p.adjust.method = "BH")
dunn_test(data, Architecture ~ Group, p.adjust.method = "BH")
dunn_test(data, Total ~ Group, p.adjust.method = "BH")
