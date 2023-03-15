setwd("~/")
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(rstatix)

data <- read.csv("cytokine_analysis_data.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)

# set theme - this is a nice one - minimalist
library('cowplot')
theme_set(theme_cowplot())

# Prep stats for error bar
data <- data %>% mutate(Group = fct_relevel(Group, "Control","ABX","pfoA+","pfoA-"))# reorder first
data <- summarySE(data, measurevar="Value", groupvars=c("Assay","Group"))


# Bar charts for all cytokines stratified by cytokines -- all data is here.
p <- 
  ggplot(data,aes(x=Group, y=Value, fill=Group)) + geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(breaks=c(0,50,100,150,200,250), limits=c(0,250)) +
  geom_errorbar(aes(ymin=Value-se, ymax=Value+se),
                width=.3,                    # Width of the error bars
                position=position_dodge(.9))+
  scale_fill_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","pfoA+"="Red2","pfoA-"="Grey"))+
  ylab("")+xlab("")+theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))

p + theme(axis.text.x = element_blank(),axis.ticks.x=element_blank()) +facet_grid(~ Assay) + ylab("")+
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))

# But it seems to difficult to visualise everything in one go as scale is different
# So filter the data according to cytokine groups, then visualise one by one.
data <- read.csv("cytokine_analysis_data.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)
data <- data %>% mutate(Group = fct_relevel(Group, "Control","ABX","pfoA+","pfoA-"))# reorder first



# Now filter subsets
IFNg <-filter(data, Assay == "IFN-g")
IL10 <-filter(data, Assay == "IL-10")
IL12p70 <-filter(data, Assay == "IL-12p70")
IL17 <-filter(data, Assay == "IL-17")
IL1b <-filter(data, Assay == "IL-1b")
IL22 <-filter(data, Assay == "IL-22")
IL4 <-filter(data, Assay == "IL-4")
IL6 <-filter(data, Assay == "IL-6")
KCGRO <-filter(data, Assay == "KC/GRO")
TNFa <-filter(data, Assay == "TNF-a")

# separate box plots for cytokines
# IFN-g

bc1 <- 
  ggplot(IFNg,aes(x=Group, y=Value, fill=Group)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = NA, notch=FALSE,varwidth = FALSE, size=0.2, width=0.8) +
  scale_y_continuous(breaks=c(0,2,4,6,8), limits=c(0,8)) +
  scale_fill_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","pfoA+"="Red2","pfoA-"="Grey"))+
  ylab("")+xlab("")+theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))+
  theme(axis.text.x = element_blank(),axis.ticks.x=element_blank()) +facet_grid(~ Assay) + ylab("")+
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1), legend.position = "bottom")
bc1

# IL-10
bc2 <- 
  ggplot(IL10,aes(x=Group, y=Value, fill=Group)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = NA, notch=FALSE,varwidth = FALSE, size=0.2, width=0.8) +
  scale_y_continuous(breaks=c(0,2,4,6,8), limits=c(0,8)) +
  scale_fill_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","pfoA+"="Red2","pfoA-"="Grey"))+
  ylab("")+xlab("")+theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))+
  theme(axis.text.x = element_blank(),axis.ticks.x=element_blank()) +facet_grid(~ Assay) + ylab("")+
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1), legend.position = "bottom")
bc2

# IL-12p70
bc3 <- 
  ggplot(IL12p70,aes(x=Group, y=Value, fill=Group))  +
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = NA, notch=FALSE,varwidth = FALSE, size=0.2, width=0.8) +
  scale_y_continuous(breaks=c(0,2,4,6,8,10,12), limits=c(0,12)) +
  scale_fill_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","pfoA+"="Red2","pfoA-"="Grey"))+
  ylab("")+xlab("")+theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))+
  theme(axis.text.x = element_blank(),axis.ticks.x=element_blank()) +facet_grid(~ Assay) + ylab("")+
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1), legend.position = "bottom")
bc3

# IL-17
bc4 <- 
  ggplot(IL17,aes(x=Group, y=Value, fill=Group)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = NA, notch=FALSE,varwidth = FALSE, size=0.2, width=0.8) +
  scale_y_continuous(breaks=c(0,2,4,6,8), limits=c(0,8)) +
  scale_fill_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","pfoA+"="Red2","pfoA-"="Grey"))+
  ylab("")+xlab("")+theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))+
  theme(axis.text.x = element_blank(),axis.ticks.x=element_blank()) +facet_grid(~ Assay) + ylab("")+
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1), legend.position = "bottom")
bc4

# IL-1b
bc5 <- 
  ggplot(IL1b,aes(x=Group, y=Value, fill=Group)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = NA, notch=FALSE,varwidth = FALSE, size=0.2, width=0.8) +
  scale_y_continuous(breaks=c(0,20,40,60,80,100), limits=c(0,100)) +
  scale_fill_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","pfoA+"="Red2","pfoA-"="Grey"))+
  ylab("")+xlab("")+theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))+
  theme(axis.text.x = element_blank(),axis.ticks.x=element_blank()) +facet_grid(~ Assay) + ylab("")+
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1), legend.position = "bottom")
bc5

# IL-22
bc6 <- 
  ggplot(IL22,aes(x=Group, y=Value, fill=Group)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = NA, notch=FALSE,varwidth = FALSE, size=0.2, width=0.8) +
  scale_y_continuous(breaks=c(0,2,4,6,8,10,12), limits=c(0,12)) +
  scale_fill_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","pfoA+"="Red2","pfoA-"="Grey"))+
  ylab("")+xlab("")+theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))+
  theme(axis.text.x = element_blank(),axis.ticks.x=element_blank()) +facet_grid(~ Assay) + ylab("")+
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1), legend.position = "bottom")
bc6

# IL-4
bc7 <- 
  ggplot(IL4,aes(x=Group, y=Value, fill=Group)) +
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = NA, notch=FALSE,varwidth = FALSE, size=0.2, width=0.8) +
  scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1.0,1.25), limits=c(0,1.25)) +
  scale_fill_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","pfoA+"="Red2","pfoA-"="Grey"))+
  ylab("")+xlab("")+theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))+
  theme(axis.text.x = element_blank(),axis.ticks.x=element_blank()) +facet_grid(~ Assay) + ylab("")+
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1), legend.position = "bottom")
bc7

# IL-6
bc8 <- 
  ggplot(IL6,aes(x=Group, y=Value, fill=Group)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = NA, notch=FALSE,varwidth = FALSE, size=0.2, width=0.8) +
  scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1.0,1.25), limits=c(0,1.25)) +
  scale_fill_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","pfoA+"="Red2","pfoA-"="Grey"))+
  ylab("")+xlab("")+theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))+
  theme(axis.text.x = element_blank(),axis.ticks.x=element_blank()) +facet_grid(~ Assay) + ylab("")+
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1), legend.position = "bottom")
bc8

# KC/GRO
bc9 <- 
  ggplot(KCGRO,aes(x=Group, y=Value, fill=Group)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = NA, notch=FALSE,varwidth = FALSE, size=0.2, width=0.8) +
  scale_y_continuous(breaks=c(0,100,200,300,400), limits=c(0,400)) +
  scale_fill_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","pfoA+"="Red2","pfoA-"="Grey"))+
  ylab("")+xlab("")+theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))+
  theme(axis.text.x = element_blank(),axis.ticks.x=element_blank()) +facet_grid(~ Assay) + ylab("")+
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1), legend.position = "bottom")
bc9

# TNF-a
bc10 <-
  ggplot(TNFa,aes(x=Group, y=Value, fill=Group)) +
#  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = 0.1, notch=FALSE,varwidth = FALSE, size=0.2) +
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = NA, notch=FALSE,varwidth = FALSE, size=0.2, width=0.8) +
  scale_y_continuous(breaks=c(0,10,20,30,40,50,60), limits=c(0,60)) +
  #geom_errorbar(aes(ymin=Value-se, ymax=Value+se),
  #              width=.3,                    # Width of the error bars
  #              position=position_dodge(.9))+
  scale_fill_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","pfoA+"="Red2","pfoA-"="Grey"))+
  ylab("")+xlab("")+theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))+
  facet_grid(~ Assay) + ylab("")+
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1), legend.position = "bottom")
#  geom_jitter(size=0.7,shape=1, width = 0.3)
bc10


## Stats
data <- read.csv("cytokine_analysis_data.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)
shapiro.test(data$Value) # surely different from normal distribution so it is non-parametric

# Sub-set the data (again, as this is separate from the graphing)
IFNg <-filter(data, Assay == "IFN-g")
IL10 <-filter(data, Assay == "IL-10")
IL12p70 <-filter(data, Assay == "IL-12p70")
IL17 <-filter(data, Assay == "IL-17")
IL1b <-filter(data, Assay == "IL-1b")
IL22 <-filter(data, Assay == "IL-22")
IL4 <-filter(data, Assay == "IL-4")
IL6 <-filter(data, Assay == "IL-6")
KCGRO <-filter(data, Assay == "KC/GRO")
TNFa <-filter(data, Assay == "TNF-a")

# Run statistical analysis
kruskal.test(Value ~ Group, data=IFNg)
kruskal.test(Value ~ Group, data=IL10)
kruskal.test(Value ~ Group, data=IL12p70)
kruskal.test(Value ~ Group, data=IL17)
kruskal.test(Value ~ Group, data=IL1b) # P=0.015
kruskal.test(Value ~ Group, data=IL22)
kruskal.test(Value ~ Group, data=IL4)
kruskal.test(Value ~ Group, data=IL6)
kruskal.test(Value ~ Group, data=KCGRO) # P=0.0005
kruskal.test(Value ~ Group, data=TNFa) # P=0.0005

# Test which group is statistically different (if overall KW test P<0.05), suitable for unequal number of observations
library(rstatix)
dunn_test(IL1b, Value ~ Group, p.adjust.method = "BH")
dunn_test(KCGRO, Value ~ Group, p.adjust.method = "BH")
dunn_test(TNFa, Value ~ Group, p.adjust.method = "BH")


