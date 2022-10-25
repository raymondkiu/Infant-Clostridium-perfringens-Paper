setwd("~/Desktop/pretermCP/Analysis-Rev1/Survivalassays/")
library(tidyverse)
library(ggpubr)

# Data format in csv
# Group	Lineage	Day	Percentage
# PFO+	Lineage 7	Day 0	100
# PFO+	Lineage 7	Day 2	9.375000023
# PFO+	Lineage 7	Day 4	2.100000005
# PFO+	Lineage 7	Day 6	0.600000002
# PFO+	Lineage 7	Day 8	0.03375
# PFO+	Lineage 7	Day 10	0.008625

data <- read.csv("OxygenL13567.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)

# Set up favourite theme, nicer than bw
library('cowplot')
theme_set(theme_cowplot())

# Reorder in plot
data$Group <- factor(data$Group, levels = c("PFO+","PFO-"))
data$Lineage <- factor(data$Lineage, levels=c("Lineage 1","Lineage 2","Lineage 3","Lineage 4","Lineage 5","Lineage 6","Lineage 7","Lineage 8"))

p <- data %>% 
  ggplot(aes(x=Time, y=Value, group=Lineage, color=Group)) +
  scale_color_manual(values=c("PFO+"="red2","PFO-"="Grey"))+
  scale_x_continuous(breaks=c(0,24,48,72,96,120,144,168,192,216,240,264,288,312,336), limits=c(0,336))+
  #scale_y_log10(limits = c(1e-6,150),breaks = trans_breaks("log10", function(x) 10^x))+
  scale_y_log10(limits = c(1e-8,150),breaks =c(100,10,1,0.1,0.01,0.001,0.0001,0.00001,0.000001),
                label = c("100", "10","1", "0.1","0.01","0.001","0.0001","0.00001","0"))+
  stat_summary(fun=mean, geom="line") # only scale_color_manual goes with stat_summary
p
p + theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))


# Do one line plot grouped in lineages 1,3,5,6,7
q <- data %>% 
  ggplot(aes(x=Time, y=Value, group=Lineage, color=Lineage)) +
  scale_color_manual(values=c("Lineage 1"="#ba1114","Lineage 3"="#ebda26","Lineage 5"="#122cc4","Lineage 6"="#00d9ff","Lineage 7"="#4b0c71"))+
  scale_x_continuous(breaks=c(0,24,48,72,96,120,144,168,192,216,240,264,288,312,336), limits=c(0,336))+
  #scale_y_log10(limits = c(1e-6,150),breaks = trans_breaks("log10", function(x) 10^x))+
  scale_y_log10(limits = c(1e-8,120),breaks =c(100,10,1,0.1,0.01,0.001,0.0001,0.00001,0.000001,0.0000001),
                label = c("100", "10","1", "0.1","0.01","0.001","0.0001","0.00001","0.000001","0"))+
  stat_summary(fun=mean, geom="line")+ # only scale_color_manual goes with stat_summary
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))
q

# Do one line plot grouped in all lineages
data <- read.csv("Oxygen3.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)
q <- data %>% 
  ggplot(aes(x=Time, y=Value, group=Lineage, color=Lineage)) +
  scale_color_manual(values=c("Lineage 1"="#ba1114","Lineage 2"="#f79ea4","Lineage 3"="#ebda26","Lineage 4"="#701501","Lineage 5"="#122cc4","Lineage 6"="#00d9ff","Lineage 7"="#4b0c71","Lineage 8"="#daaff3"))+
  scale_x_continuous(breaks=c(0,24,48,72,96,120,144,168,192,216,240,264,288,312,336), limits=c(0,336))+
  #scale_y_log10(limits = c(1e-6,150),breaks = trans_breaks("log10", function(x) 10^x))+
  scale_y_log10(limits = c(1e-8,120),breaks =c(100,10,1,0.1,0.01,0.001,0.0001,0.00001,0.000001,0.0000001),
                label = c("100", "10","1", "0.1","0.01","0.001","0.0001","0.00001","0.000001","0"))+
  stat_summary(fun=mean, geom="line")+ # only scale_color_manual goes with stat_summary
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))
q







# p + facet_grid( ~ Group, scales="free")+ theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))

# Figure for groups only in boxplots:
Data <- read.csv("Oxygen34-graph.csv", header = TRUE, stringsAsFactors = TRUE)
head(Data)

# Reorder groups
Data$Group <- factor(Data$Group, levels = c("PFO+","PFO-"))
Data$Lineage <- factor(Data$Lineage, levels=c("Lineage 1","Lineage 3","Lineage 5","Lineage 6","Lineage 7"))

# Set Time as factor so it will treat numeric data as string - this will work:
Data$Time <- factor(Data$Time, levels=c("0","48","96","144","192","240","288","336"))
p <- 
  ggplot(Data,aes(x=Time, y=Value)) +
  geom_boxplot(aes(fill=Lineage),outlier.colour = "black", outlier.shape = NA, outlier.alpha = 0.2, notch=FALSE,varwidth = FALSE, size=0.2) +
  scale_y_log10(limits = c(1e-8,150),breaks =c(100,10,1,0.1,0.01,0.001,0.0001,0.00001,0.000001,0.0000001),
                label = c("100", "10","1", "0.1","0.01","0.001","0.0001","0.00001","0.000001","0"))
p+scale_fill_manual(values=c("Lineage 1"="#ba1114","Lineage 3"="#ebda26","Lineage 5"="#122cc4","Lineage 6"="#00d9ff","Lineage 7"="#4b0c71")) + 
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))

# For stats on 336h only use Kruskal Wallis, non-parametric alternative of One-way ANOVA:
library(FSA)
Summarize(Value~ Time, data = Data)

Data <- read.csv("Oxygen34-stats.csv", header = TRUE, stringsAsFactors = TRUE)
head(Data)

# test if the data is normally distributed, run Kruskal-Wallis as not normal
shapiro.test(Data$Value)

h48 <- filter(Data, Time== "48")
h96 <- filter(Data, Time== "96")
h144 <- filter(Data, Time== "144")
h192 <- filter(Data, Time== "192")
h240 <- filter(Data, Time== "240")
h288 <- filter(Data, Time== "288")
h336 <- filter(Data, Time== "336")

# run Kurskal Wallis as data is normally distributed
kruskal.test(Value ~ Lineage, data=h48)
kruskal.test(Value ~ Lineage, data=h96)
kruskal.test(Value ~ Lineage, data=h144)
kruskal.test(Value ~ Lineage, data=h192)
kruskal.test(Value ~ Lineage, data=h240)
kruskal.test(Value ~ Lineage, data=h288)
kruskal.test(Value ~ Lineage, data=h336)

# mutliple comparison for those groups tested significant
library(rstatix)
dt48 <-dunn_test(Value ~ Lineage, data=h48,p.adjust.method="holm")
dt96 <-dunn_test(Value ~ Lineage, data=h96,p.adjust.method="holm")
dt144 <-dunn_test(Value ~ Lineage, data=h144,p.adjust.method="holm")
dt192 <-dunn_test(Value ~ Lineage, data=h192,p.adjust.method="holm")

print(dt48, n=40)
print(dt96, n=40)
print(dt144, n=40)
print(dt192, n=40)


