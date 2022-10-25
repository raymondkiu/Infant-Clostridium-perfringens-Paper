setwd("~/Desktop/pretermCP/Analysis-Rev1/Figure2/")
library(ggplot2)
library(tidyverse)
library(ggpubr)

#data <- read.csv("dotplot-data.csv",sep =";", header = TRUE, stringsAsFactors = FALSE)
# compared lineage 2356 vs lineage 4 only
data <- read.csv("Virulence80-2.csv", header = TRUE, stringsAsFactors = TRUE)
data
#data$ID <- factor(data$ID, labels = c("A12356", "B4"))
#data

# compare all individual lineages
# data2 <- read.csv("VirulenceCountptnew.csv", header = TRUE, stringsAsFactors = FALSE)
# data2
# data2$ID <- factor(data2$ID, labels = c("1", "2", "3", "4", "5","6"))

# Basic box plot
library('cowplot')
theme_set(theme_cowplot())

p <- 
 data %>%
# mutate(ID = fct_relevel(ID, "A12356","B4")) %>% 
  ggplot(aes(x=Lineage, y=Virulence, fill=Lineage)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = 0.1, notch=FALSE,varwidth = FALSE, size=0.2) +
  scale_y_continuous(limits=c(0, 25)) + geom_jitter(width = 0.05, alpha=0.1)
p
p+ scale_fill_manual(values=c("I"="#ba1114","III"="#ebda26","V"="#122cc4","VI"="#00d9ff","VII"="#4b0c71"))
#p+scale_fill_manual(values=c("#BA1114","#F79EA4","#EBDA26","#701501","#00D9FF","#122CC4"))

# get summary
data
library(Rmisc)
summarySE(data, measurevar="Virulence", groupvars=c("Lineage"))

# colonisation plot
data <- read.csv("Colonisation80.csv", header = TRUE, stringsAsFactors = TRUE)
data
p <- ggplot(data, aes(x=Lineage, y=Colonisation, fill=Lineage)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = 0.1, notch=FALSE,varwidth = FALSE, size=0.2) +
  scale_y_continuous(breaks = c(0,2,4,6,8,10,12,14,16),limits=c(0, 15)) + geom_jitter(width = 0.05, alpha=0.1)
p
p+ scale_fill_manual(values=c("I"="#ba1114","III"="#ebda26","V"="#122cc4","VI"="#00d9ff","VII"="#4b0c71"))
#p+scale_fill_manual(values=c("#BA1114","#F79EA4","#EBDA26","#701501","#00D9FF","#122CC4"))
p


#geom_jitter(width = 0.05, alpha=0.2)
# Notched box plot
#ggplot(data, aes(x=SUM, y=ID)) + 
#  geom_boxplot(notch=TRUE)

#p + geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
#p + geom_jitter(shape=16, position=position_jitter(0.2))
#p + stat_summary(fun.y=mean, geom="point", shape=23, size=4)


# Make pie charts - toxinomes
##############################################
data <- read.csv("virulence-perc.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)

# melt data
#plotDat <- reshape::melt(data)
#plotDat <- as.data.frame(plotDat)
#print(plotDat)

# filter data
piechart1<- filter (data,Lineage == "VII")
head(piechart1)
# plot bar chart first then convert
ToxinPalette <- c( "plc"="skyblue","pfoA"="red","ccp"="royalblue","colA"="slateblue","cpe"="yellow","etx"="black","alv"="grey","iap/ibp"="rosybrown","becA/B"="orange","cpb2"="plum")

pc1<- ggplot(piechart1, aes(x="", y=Perc, fill=Toxin))+
  scale_fill_manual(values=ToxinPalette)+
  geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)+
  theme(axis.text.x=element_blank(),axis.line.x = element_blank(),axis.line.y=element_blank(),axis.ticks=element_blank(),legend.position="right")+
  ylab("") + xlab("")

pc1

# pie chart -> child/neonate %

data <- read.csv("childneonate.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)

# filter data
piechart1<- filter (data,Lineage == "VIII")
head(piechart1)
# plot bar chart first then convert
ChildPalette <- c( "Child"="#86eba3","Neonate"="#eb021a")

pc1<- ggplot(piechart1, aes(x="", y=Perc, fill=Sample))+
  scale_fill_manual(values=ChildPalette)+
  geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)+
  theme(axis.text.x=element_blank(),axis.line.x = element_blank(),axis.line.y=element_blank(),axis.ticks=element_blank(),legend.position="right")+
  ylab("") + xlab("")

pc1

# Pie charts for hospital sources for lineages V, VI, VII
data <- read.csv("source-proportion.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)
#Set colour
SourcePalette <- c( "Hospital"="gold","Others"="lightgrey")
# filter data
piechart1<- filter (data,Lineage == "VII")

pc2<- ggplot(piechart1, aes(x="", y=Perc, fill=Source))+
  scale_fill_manual(values=SourcePalette)+
  geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)+
  theme(axis.text.x=element_blank(),axis.line.x = element_blank(),axis.line.y=element_blank(),axis.ticks=element_blank(),legend.position="right")+
  ylab("") + xlab("")

pc2


# barplots for toxins pfoA, cpb2 and cpe in each lineage
# Inputs:
#Lineage	Perc
#I	100
#III	100
#V	19
#VI	67
#VII	100
data <- read.csv("BarCpb2.csv")
data
bc <- ggplot(data, aes(x=Lineage, y=Perc),position_stack(reverse = TRUE))+
  geom_bar(stat = "identity",fill="plum")+
  ylab("") + xlab("")+
  scale_y_continuous(breaks=c(0,25,50,75,100), limits=c(0,100))+ theme(legend.position = "none")
bc

# Not great enough, do a dot/bubble plot for all toxin genes across each lineage
####################
data <- read.csv("DotToxinAll.csv", header = TRUE, stringsAsFactors = TRUE)
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

# Do a heatmap for CPA-NEC strains - to show they carry pfoA genes and other toxin gens
setwd("~/Desktop/pretermCP/Analysis-Rev1/Figure2/")
data <- read.csv("NECstrainHeatmap.csv")
# input:
# Group	Assay	Value
# Bell Stage II/III	Necrosis	7
# Bell Stage II/III	Necrosis	5
# Bell Stage II/III	Necrosis	4
# Bell Stage II/III	Necrosis	5
# Bell Stage I	Necrosis	2
data$group<-cut(data$Value,
                 breaks = c(-2,0.5,0.8,1))

# apply theme
library('cowplot')
theme_set(theme_cowplot())

TGP <-c("Yes"="#e5484d","No"="White")

ggplot(data=data) +
  geom_tile(aes(x=Strain, y=Toxin,fill=group),colour="white",size=4) +
  #theme_void() +
#  scale_fill_discrete(values=c("red","blue")) +
  scale_fill_manual(breaks = levels(data$group),
                    values = c("white","white","#e5484d"))+
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))

# STATS - virulence
Data = data
Data
kruskal.test(Virulence ~ Lineage, data = Data)

library(FSA)
dunnTest(Virulence ~ Lineage, data = Data,method="holm")
#dunn_test(Data$Colonisation, Data$ID, p.adjust.methods="bonferroni")

# STATS - colonisation factor
Data = data
head(Data)
kruskal.test(Colonisation ~ Lineage, data = Data)
wilcox.test(SUM ~ ID, data = Data)

library(FSA)
dunnTest(Colonisation ~ Lineage, data = Data,method="holm")

#dunn_test(Data$SUM, Data$ID, p.adjust.methods="bonferroni")

# STATS - toxin pfoA, cpe, cpb2
# Fisher exact test on contigency tables 2x5
setwd("~/Desktop/pretermCP/Analysis-Rev1/Figure2")
matrix <- as.matrix(read.csv("Fishercpb2.csv",header=TRUE, 
                             row.names=1))
matrix
fisher.test(matrix)
# posthoc test for chisquare = if p value <0.05, then it is associated with a lineage, if residuals >+2 
# it is a major contributor, if <-2 it is a weak contributor.
library(rcompanion) # for fisher post hoc
# inputs:
#Lineage	Yes	No
#I	2	3
#III	1	4
#V	6	29
#VI	9	3
#VII	12	5
pairwiseNominalIndependence(matrix,fisher = TRUE,compare="row",
                            gtest  = FALSE,
                            chisq  = FALSE,
                            digits = 3,
                            method = "holm")
## source porportion: is it significantly enriched?
setwd("~/Desktop/pretermCP/Analysis-Rev1/Figure2")
matrix <- as.matrix(read.csv("source-proportion-stats.csv",header=TRUE, 
                             row.names=1))
fisher.test(matrix)
pairwiseNominalIndependence(matrix,fisher = TRUE,compare="row",
                            gtest  = FALSE,
                            chisq  = FALSE,
                            digits = 3,
                            method = "holm")
# Fisher test for child/neonate ratio 2x5 contigency table
# input format:
#Lineage	Yes	No
#I	2	3
#III	1	4
#V	6	29
#VI	9	3
#VII	12	5
matrix <- as.matrix(read.csv("Fisherchildneonate.csv",header=TRUE, row.names=1))
matrix
fisher.test(matrix) # if significant, do post hoc
pairwiseNominalIndependence(matrix,fisher = TRUE,
                            gtest  = FALSE,
                            chisq  = FALSE,
                            digits = 5,
                            method = "holm") # this is Fisher post hoc test from companion package



