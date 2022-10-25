setwd("~/Desktop/pretermCP/Analysis-Rev1/Figure4/")
library(ggplot2)
library(tidyverse)
library(ggpubr)


# Set theme
library('cowplot')
theme_set(theme_cowplot())

data <- read.csv("Plasmid-proportion-lineage.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)

# filter data
piechart1<- filter (data,Lineage == "VII")
head(piechart1)
# plot bar chart first then convert
PlasmidPalette <- c( "pCW3"="#e50534","pCP13"="#6600ff","Both"="Gold","No"="lightgrey")

pc1<- ggplot(piechart1, aes(x="", y=Value, fill=Plasmid))+
  scale_fill_manual(values=PlasmidPalette)+
  geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)+
  theme(axis.text.x=element_blank(),axis.line.x = element_blank(),axis.line.y=element_blank(),axis.ticks=element_blank(),legend.position="right")+
  ylab("") + xlab("")

pc1


# STATS
# Fisher exact test on contigency tables any size
setwd("~/Desktop/pretermCP/Analysis-Rev1/Figure4/")
matrix <- as.matrix(read.csv("Plasmid-proportion-stats.csv",header=TRUE, 
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


## NEC vs non-NEC bar chart
data <- read.csv("NECvsNonNEC.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)

# filter data
NECPalette <- c( "Yes"="purple","No"="grey90")

data$Lineage <- factor(data$Lineage, levels=c("Lineage V", "CPA-NEC", "Non-CPA-NEC"))

bc1 <- ggplot(data, aes(x=factor(Lineage), y=Perc, fill=factor(Plasmid)),position_stack(reverse = TRUE))+
  scale_fill_manual(values =NECPalette)+
  geom_bar(stat = "identity")+
  ylab("") + xlab("")+
  scale_y_continuous(breaks=c(0,25,50,75,100), limits=c(0,100)) + theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))
#  scale_x_discrete(labels=c("QCCH" = "A", "SMH" = "B",
#                            "NNUH" = "C","RH"="D","NUTH"="E"))

bc1

# STATS
# Fisher exact test on contigency tables any size
setwd("~/Desktop/pretermCP/Analysis-Rev1/Figure4/")
matrix <- as.matrix(read.csv("NECvsNonNEC-stats.csv",header=TRUE, 
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

