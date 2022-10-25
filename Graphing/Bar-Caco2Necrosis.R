setwd("~/Desktop/pretermCP/invitro/Necrosis_assay_Caco2/")
library(tidyverse)
library(ggpubr)

# Data format in csv

#Sample	Isolate	Group	Necrosis
#Q215	IQ146	Bell Stage II/III	28.58405
#Q215	IQ146	Bell Stage II/III	29.40246
#Q215	IQ146	Bell Stage II/III	28.80725
#Q088	IQ036	Bell Stage II/III	15.26622
#Q088	IQ036	Bell Stage II/III	19.53964
#Q088	IQ036	Bell Stage II/III	20.51616
#Q143	IQ129	Bell Stage II/III	18.98163
#Q143	IQ129	Bell Stage II/III	17.17275


#data <- read.csv("dotplot-data.csv",sep =";", header = TRUE, stringsAsFactors = FALSE)
data <- read.csv("Necros-Caco2v2.0.csv", header = TRUE, stringsAsFactors = TRUE)
data

# Basic box plot
library('cowplot')
theme_set(theme_cowplot())

# Summary - make a statistical summary for se,sd and ci for error bars
#install.packages('Rmisc', dependencies = TRUE)
library(Rmisc)
data <- summarySE(data, measurevar="Necrosis", groupvars=c("Group","Isolate","Assay")) # Group first then Isolate
data

# Reorder only group
data$Group <- factor(data$Group, levels = c("Bell Stage II/III", "Bell Stage I", "Non-NEC","Control"))
data$Assay <- factor(data$Assay, levels=c("Necrosis Assay"))
data
# Reorder labeling using fct_relevel
# Caco2 Necrosis experiment
p <- data %>%
  mutate(Isolate = fct_relevel(Isolate, 
                               "IQ146", "IQ036", "IQ129", 
                               "IQ024", "LH019", "IQ153", 
                               "LH115", "LH043","IQ147","IQ133","Control")) %>% 
  ggplot(aes(x=Isolate, y=Necrosis, fill=Group)) + geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(breaks=c(0, 5, 10,15,20,25,30,35,40,45,50), limits=c(-1,35)) +
  geom_errorbar(aes(ymin=Necrosis-se, ymax=Necrosis+se),
                width=.3,                    # Width of the error bars
                position=position_dodge(.9))
p

# Change colour for groups
p+
  scale_fill_manual(values=c("red","gold","darkturquoise","grey")) + 
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))+
  facet_grid( ~ Assay, scales="free")

# Figure for groups only
data <- summarySE(data, measurevar="Necrosis", groupvars=c("Group","Assay")) # Group first then Isolate
data

p <- 
  ggplot(data,aes(x=Group, y=Necrosis, fill=Group)) + geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(breaks=c(0, 5, 10,15,20,25,30,35,40,45,50), limits=c(-1,35)) +
  geom_errorbar(aes(ymin=Necrosis-se, ymax=Necrosis+se),
                width=.3,                    # Width of the error bars
                position=position_dodge(.9))
p

p+scale_fill_manual(values=c("red","gold","darkturquoise","grey")) + 
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))+
  facet_grid( ~ Assay, scales="free")

# stats: use Kruskal-Wallis, non-parametric on which isolate is significantly different
Data <- read.csv("Necros-Caco2-isolateStats.csv", header = TRUE, stringsAsFactors = TRUE)
Data
library(FSA)
Summarize(Necrosis~ Isolate, data = Data)

res.kruskal <- kruskal.test(Necrosis ~ Isolate, data=Data)
res.kruskal

# Test which group is statistically different, suitable for unequal number of observations
dunnTest(Data$Necrosis, Data$Isolate,method="none")

# Test which group is significant Bell II/III vs Bell I vs non-NEC vs Control
Data <- read.csv("Necros-Caco2-GroupStats.csv", header = TRUE, stringsAsFactors = TRUE)
Data
library(FSA)
Summarize(Necrosis~ Group, data = Data)

res.kruskal <- kruskal.test(Necrosis ~ Group, data=Data)
res.kruskal

# Test which group is statistically different, suitable for unequal number of observations
dunnTest(Data$Necrosis, Data$Group,method="none")

