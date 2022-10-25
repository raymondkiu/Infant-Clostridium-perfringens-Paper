setwd("~/Desktop/pretermCP/Analysis-Rev1/InVitro/Cytotoxicity/")
library(tidyverse)
library(ggpubr)

# Data format in csv
#Sample	Isolate	Group	Necrosis
#Q215	IQ146	Bell Stage II/III	28.58405
#Q215	IQ146	Bell Stage II/III	29.40246

#data <- read.csv("dotplot-data.csv",sep =";", header = TRUE, stringsAsFactors = FALSE)
data <- read.csv("Cytotoxicity.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)

# Basic box plot
library('cowplot')
theme_set(theme_cowplot())

# Summary - make a statistical summary for se,sd and ci for error bars
#install.packages('Rmisc', dependencies = TRUE)
library(Rmisc)
data <- summarySE(data, measurevar="Perc", groupvars=c("Strain","Lineage","Group")) # Group first then Isolate
data

# Reorder only group
#data$Group <- factor(data$Group, levels = c("Untreated", "PFO+","PFO-"))

# Caco2 Necrosis experiment
p <- data %>%
  ggplot(aes(x=Strain, y=Perc, fill=Group)) + geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(breaks=c(0, 10,20,30,40,50,60,70), limits=c(-1,70)) +
  geom_errorbar(aes(ymin=Perc-se, ymax=Perc+se),
                width=.3,                    # Width of the error bars
                position=position_dodge(.9))
p + theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))
 # facet_grid( ~ Group, scales="free")


### Figure for groups only: barplot (final)
# Grouped by PFO
data$Group <- factor(data$Group, levels = c("Untreated", "pfoA+","pfoA-"))
data <- summarySE(data, measurevar="Perc", groupvars=c("Group")) # Group first then Isolate
data

p <- 
  ggplot(data,aes(x=Group, y=Perc, fill=Group)) + geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(breaks=c(0, 10,20,30,40), limits=c(-1,40)) +
  geom_errorbar(aes(ymin=Perc-se, ymax=Perc+se),
                width=.3,                    # Width of the error bars
                position=position_dodge(.9)) +
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))+ xlab ("") + ylab("")+
  scale_fill_manual(values=c("Control"="lightgreen","pfoA+"="Red2","pfoA-"="Grey"))
p

# Grouped by lineages
#scale_fill_manual(values=c("Lineage 1"="#ba1114","Lineage 3"="#ebda26",
# "Lineage 5"="#122cc4","Lineage 6"="#00d9ff","Lineage 7"="#4b0c71"))+
data <- read.csv("CytotoxicityNew.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)
data$Lineage <- factor(data$Lineage, levels = c("Untreated", "I","III","V","VI","VII"))
data <- summarySE(data, measurevar="Perc", groupvars=c("Lineage")) # Group first then Isolate
data

p <- 
  ggplot(data,aes(x=Lineage, y=Perc, fill=Lineage)) + geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(breaks=c(0, 10,20,30,40,50), limits=c(-2,50)) +
  geom_errorbar(aes(ymin=Perc-se, ymax=Perc+se),
                width=.3,                    # Width of the error bars
                position=position_dodge(.9)) +
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))+ xlab("") +ylab("")+
  scale_fill_manual(values=c("I"="#ba1114","III"="#ebda26","V"="#122cc4","VI"="#00d9ff","VII"="#4b0c71"))
p

# No groups -> All strains - to be joined by a heatmap of toxin genes
data <- read.csv("Cytotoxicity.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)
data$Group <- factor(data$Group, levels = c("PFO+", "PFO-","Untreated"))
data$Lineage <- factor(data$Lineage, levels= c("I","II","III","IV","V","VI","VII","VIII"))
data <- summarySE(data, measurevar="Perc", groupvars=c("Strain","Group","Lineage")) # Group first then Isolate
data


p <- 
  ggplot(data,aes(x=Strain, y=Perc, fill=Lineage)) + geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70), limits=c(-1,70)) +
  geom_errorbar(aes(ymin=Perc-se, ymax=Perc+se),
               width=.3,                    # Width of the error bars
                position=position_dodge(.9)) +
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1),axis.ticks.x = element_blank())+ xlab("") +ylab("")+
  facet_grid( ~ Group, scales="free")+
  #scale_fill_manual(values=c("Control"="lightgreen","PFO+"="Red2","PFO-"="Grey"))
  scale_fill_manual(values=c("I"="#ba1114","II"="#f79ea4","III"="#ebda26","IV"="#701501","V"="#122cc4","VI"="#00d9ff","VII"="#4b0c71","VIII"="#daaff3"))
p

# Get lineage colour block
data <- read.csv("Cytotoxicity-colourblock.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)
data$Group <- factor(data$Group, levels = c("PFO+", "PFO-","Untreated"))
data$Lineage <- factor(data$Lineage, levels= c("I","II","III","IV","V","VI","VII","VIII"))
data <- summarySE(data, measurevar="Perc", groupvars=c("Strain","Group","Lineage")) # Group first then Isolate
data

p <- 
  ggplot(data,aes(x=Strain, y=Perc, fill=Lineage)) + geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(breaks=c(0,10,20,30), limits=c(-1,30)) +
  geom_errorbar(aes(ymin=Perc-se, ymax=Perc+se),
                width=.3,                    # Width of the error bars
                position=position_dodge(.9)) +
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1),axis.ticks.x = element_blank())+ xlab("") +ylab("")+
  facet_grid( ~ Group, scales="free")+
  #scale_fill_manual(values=c("Control"="lightgreen","PFO+"="Red2","PFO-"="Grey"))
  scale_fill_manual(values=c("I"="#ba1114","II"="#f79ea4","III"="#ebda26","IV"="#701501","V"="#122cc4","VI"="#00d9ff","VII"="#4b0c71","VIII"="#daaff3"))
p

## toxin profile heatmap using ggplot2
setwd("~/Desktop/pretermCP/Analysis-Rev1/InVitro/Cytotoxicity/")
data <- read.csv("toxinprofilePFO-.csv")  # change to PFO+ or PFO- to access two different csv data files
head(data)
# Melt table first
plotDat <- reshape::melt(data, id=c("Strain","Group"))
head(plotDat)
# input e.g.:
# Group	Assay	Value
# Bell Stage II/III	Necrosis	1


# apply theme
library('cowplot')
theme_set(theme_cowplot())

## use geom_tile to build heatmap, better than heatmap2, but cannot apply clustering method:
#data$Assay <- factor(data$Assay, levels=c("Necrosis","Apoptosis","H2S","Gas","Haemolysis","Sporulation","Aerotolerance","Generation","Bile","AMR"))
p<-ggplot(data=plotDat) +
  scale_fill_gradient2(low = "white",high="#e5484d")+
#  scale_fill_gradient2(low = "white",mid="purple1",midpoint=4,high = "purple4") +
  geom_tile(aes(x=Strain, y=variable,fill=value,group=Group),color="lightgrey",size=0.5) + theme_void() +
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1),legend.position = 'left' )
 # theme(axis.text.x = element_blank(),legend.position = 'none' )
p

# Grouped by NEC to compare if NEC pfo+ is greater than non-NEC pfo+
data <- read.csv("CytotoxicityNEC.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)
data$Lineage <- factor(data$Lineage, levels = c("I","II","III","IV","V","VI","VII","VIII"))
data$Group2 <- factor(data$Group2, levels = c("CPA-NEC","Non-CPA-NEC"))
data <- summarySE(data, measurevar="Perc", groupvars=c("Group2")) # Group first then Isolate
data

p <- 
  ggplot(data,aes(x=Group2, y=Perc, fill=Group2)) + geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(breaks=c(0, 10,20,30,40,50,60,70), limits=c(-2,70)) +
  geom_errorbar(aes(ymin=Perc-se, ymax=Perc+se),
                width=.3,                    # Width of the error bars
                position=position_dodge(.9)) +
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))+ xlab("") +ylab("")+
  scale_fill_manual(values=c("CPA-NEC"="mediumpurple1","Non-CPA-NEC"="grey"))
p

# Grouped by PFoA+cpb2 to compare if pfo+cpb2 has a synergistic effect 
library(Rmisc)
library('cowplot')
theme_set(theme_cowplot())
data <- read.csv("CytotoxicityPFO+CPB2.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)
data$Lineage <- factor(data$Lineage, levels = c("I","II","III","IV","V","VI","VII","VIII"))
data$Group <- factor(data$Group, levels = c("pfoA+","pfoA+cpb2+"))
data <- summarySE(data, measurevar="Perc", groupvars=c("Group")) # Group first then Isolate
data

p <- 
  ggplot(data,aes(x=Group, y=Perc, fill=Group)) + geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70), limits=c(-2,70)) +
  geom_errorbar(aes(ymin=Perc-se, ymax=Perc+se),
                width=.3,                    # Width of the error bars
                position=position_dodge(.9)) +
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1,face = "italic"))+ xlab("") +ylab("")+
  scale_fill_manual(values=c("pfoA+cpb2+"="darkturquoise","pfoA+"="red2"))

p

# stats for pfoA+cpb2+ = 2 groups so use Wilcox (t-test for parametric test)
wilcox.test( Perc ~ Group, data = data)


# Inverse Bar chart under the heatmap

# Reorder data
# data$Group <- factor(data$Group, levels = c("Bell Stage II/III", "Bell Stage I", "Non-NEC","Negative","Positive"))
# data$Assay <- factor(data$Assay, levels=c("Necrosis","Apoptosis","H2S","Gas","Haemolysis","Sporulation","Aerotolerance","Generation","Bile","AMR"))
# data
# 
# p <- 
#   ggplot(data,aes(x=Sample, y=Value, fill=Group)) + geom_bar(position="stack", stat="identity") +
#   scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40), limits=c(0,40)) 
# 
# p
# 
# # Change colour for groups
#p+scale_y_reverse(breaks=c(35,30,25,20,15,10,5,0), lim=c(35,0))+
#  scale_fill_manual(values=c("red","gold","darkturquoise")) + 
#  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1),axis.ticks = element_blank()) 
#facet_grid( ~ Group, scales="free")


# To plot a bar chart visualised by groups, need to re-sort the data into a new csv:
# input:
# Sample	Group	Value
# IQ146	Bell Stage II/III	30
# IQ036	Bell Stage II/III	22
# IQ129	Bell Stage II/III	28
# IQ024	Bell Stage II/III	28
# LH019	Bell Stage I	10
# IQ153	Bell Stage I	23
# LH115	Bell Stage I	26
# LH043	Non-NEC	13
# IQ147	Non-NEC	6
# IQ133	Non-NEC	10
# data <- read.csv("OverallVirulenceGroupStat.csv")
# data <- summarySE(data, measurevar="Value", groupvars=c("Group")) # Group and assay otherwise assay will not be taken into account
# data
# 
# # Reorder data
# data$Group <- factor(data$Group, levels = c("Bell Stage II/III", "Bell Stage I", "Non-NEC","Negative","Positive"))
# data$Assay <- factor(data$Assay, levels=c("Necrosis","Apoptosis","H2S","Gas","Haemolysis","Sporulation","Aerotolerance","Generation","Bile","AMR"))
# data
# 
# p <- 
#   ggplot(data,aes(x=Group, y=Value, fill=Group)) + geom_bar(position="dodge", stat="identity") +
#   scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35), limits=c(0,35)) +
#   geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd),
#                 width=.3,                    # Width of the error bars
#                 position=position_dodge(.9))
# p
# 
# p+
#   scale_fill_manual(values=c("red","gold","darkturquoise")) + 
#   theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1),plot.title = element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank()) 


##########
#### STATS
##########
# to do stats in between lineages:
data <- read.csv("Cytotoxicity.csv")
head(data)
library(FSA)
options(dplyr.print_max = 1e9) # print all possible tibble rows, for dunn_test mainly
Summarize(Perc~ Lineage, data = data)
# Any difference in stats -> normality test, if normal use ANOVA if not use Kruskal Wallis?
shapiro.test(data$Perc)

res.kruskal <- kruskal.test(Perc ~ Lineage, data=data)
res.kruskal

## Test which lineage is statistically different, suitable for unequal number of observations
dunnTest(data$Perc, data$Lineage,method="bh")

dunn_test(data, Perc ~ Lineage, p.adjust.method = "BH") # better package to visualise stats

## See differences in between PFO groups?
res.kruskal <- kruskal.test(Perc ~ Group, data=data)
res.kruskal

# Test which group is statistically different, suitable for unequal number of observations
dunnTest(data$Perc, data$Group,method="bh")
dunn_test(data, Perc ~ Group, p.adjust.method = "BH")
Summarize(Perc~ Group, data = data)

# See which strain is different from untreated
res.kruskal <- kruskal.test(Perc ~ Strain, data=data)
res.kruskal
dunnTest(data$Perc, data$Strain,method="bh")
# use this options(max.print=1000000) to print more rows (unlimited)

# point-biserial correlation to correlate cell death percentage with toxin gene profile
# PfoA gene only:
Data <- read.csv("point-biserial.csv", header = TRUE, stringsAsFactors = TRUE)
Data

# Input format (binary for toxin gene presence)
#Strain plc pfoA CellDeathMean
#IQ146 1 1 50

# Store into variable, x is binomial, y is continuous variable
x <- Data$alv
x
y <- Data$Perc
y

# use Spearman for ranking if data is not normal.
cor.test(x, y,method="spearman",alternative="two.sided",exact=FALSE)

# Explanation:, correlation coefficient is 0.85, positive means that if x is 1, y tends to be higher. and P value
# is 0.001, which is <0.05, significant, so the correlation is significant.

## Stats for NEC pfo+ vs non-NEC pfo-
data <- read.csv("CytotoxicityNEC.csv")
head(data)
library(FSA)
Summarize(Perc~ Group2, data = data)
# Any difference in stats -> normality test, if normal use ANOVA if not use Kruskal Wallis?
shapiro.test(data$Perc)

wilcox.test(Perc ~ Group2, data=data, alternative="two.sided")


