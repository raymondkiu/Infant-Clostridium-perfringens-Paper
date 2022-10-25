setwd("~/Desktop/pretermCP/Analysis-Rev1/InvivoHistology/")
library(tidyverse)
library(reshape2) # for melt function
library(Rmisc) # for summarySE
data <- read.csv("weight-change-melted-group.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)

#data1 <- melt(data,  id.vars ="Group" ,  variable.name ="Day")
#data1
#write.csv(data1,"weight-change-melted.csv")
# Input format
# Time	Group	Value
# Day 0	CP	0
# Day 0	CP	0
# Day 0	CP	0
# Day 0	CP	0
# Day 0	CP	0
# Day 0	CP	0
# Day 0	CP	0
# Day 0	CP	0
# Day 1	CP	4.268293
# Day 1	CP?	5.369128

# Use theme cowplot - minimalist
library('cowplot')
theme_set(theme_cowplot())

# reorder -> not right need to check
#data1$Group <- factor(data1$Group, levels = c("Control", "ABX","IQ146","IQ036","LH115","IQ147","IQ133","LH043"))
#data1$Group <- factor(data1$Group, levels = c("Control", "ABX","PFO+","PFO-"))



# Prep stats for error bar
data <- summarySE(data, measurevar="value", groupvars=c("Group","Day"))
data

p <- data %>% mutate(Group = fct_relevel(Group, "Control","ABX","pfoA+","pfoA-")) %>%
#p <- data %>% mutate(Group2 = fct_relevel(Grou, "Untreated","ABX","IQ146","IQ036","LH115","IQ147","IQ133","LH043")) %>% 
 ggplot(aes(x=Day, y=value, color=Group, group=Group)) +
  geom_line(size=0.6)+
 #scale_color_manual(values=c("Untreated"="lightgreen","ABX"="mediumpurple1","IQ146"="red","IQ036"="orange","LH115"="pink","LH043"="navy","IQ147"="royalblue","IQ133"="lightblue"))+
  scale_color_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","pfoA+"="Red2","pfoA-"="Grey"))+
  scale_y_continuous(breaks=c(-2,-1,0,1,2,3,4,5), limits=c(-2,6))+
  stat_summary(fun=mean, geom="line") + # only scale_color_manual goes with stat_summary
  geom_errorbar(size=0.6, aes(ymin=value-se, ymax=value+se),
               width=.5)                # Width of the error bar

p + theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))
p + theme(axis.text.x = element_blank(),axis.ticks.x=element_blank()) +facet_grid(~ Group) + ylab("Weight Changes (%)")
#theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))

## Stats
Data <- read.csv("weight-change-melted-group.csv", header = TRUE, stringsAsFactors = TRUE)
head(Data)
shapiro.test(Data$value) # seem to be different from normal distribution

# filter subsets
Day1 <-filter(Data, Day == "Day 1")
Day2 <-filter(Data, Day == "Day 2")
Day3 <-filter(Data, Day == "Day 3")
Day4 <-filter(Data, Day == "Day 4")
Day5 <-filter(Data, Day == "Day 5")
Day6 <-filter(Data, Day == "Day 6")

# Run statistical analysis

library(FSA)
Summarize(value~ Group, data = Day1)
Summarize(value~ Group, data = Day2)
Summarize(value~ Group, data = Day3)
Summarize(value~ Group, data = Day4)
Summarize(value~ Group, data = Day5)
Summarize(value~ Group, data = Day6)

kruskal.test(value ~ Group, data=Day1)
kruskal.test(value ~ Group, data=Day2)
kruskal.test(value ~ Group, data=Day3)
kruskal.test(value ~ Group, data=Day4)
kruskal.test(value ~ Group, data=Day5)
kruskal.test(value ~ Group, data=Day6)

# Test which group is statistically different (if overall KW test P<0.05), suitable for unequal number of observations
library(rstatix)
dunn_test(Day1, value ~ Group, p.adjust.method = "BH")
dunn_test(Day2, value ~ Group, p.adjust.method = "BH")

#dunnTest(Day1$value, Day1$Group,method="none")
#dunnTest(Day2$value, Day2$Group,method="none")


## Test whether average weight changes are different?
setwd("~/Desktop/pretermCP/Analysis-Rev1/InvivoHistology/")
Data <- read.csv("Weight-change-average.csv", header = TRUE, stringsAsFactors = TRUE)
head(Data)


Summarize(value~ Group, data = Data)
kruskal.test(value ~ Group, data=Data)
dunnTest(Data$value, Data$Group,method="none")
dunn_test(Data, value ~ Group, p.adjust.method = "none")
## Mean weight changes - boxplot

#data <- summarySE(Data, measurevar="value", groupvars=c("Group"))
#data

p <- 
  Data %>%
  # mutate(Group = fct_relevel(Group, "Untreated","ABX","IQ146","IQ036","LH115","IQ147","IQ133","LH043")) %>% ggplot(aes(x=Group, y=Weight, fill=Group))+
  mutate(Group = fct_relevel(Group, "Control","ABX","PFO+","PFO-")) %>% ggplot(aes(x=Group, y=value, fill=Group)) + 
  geom_boxplot(outlier.colour = "white", outlier.shape = NA, outlier.alpha = 0, notch=FALSE,varwidth = FALSE, size=0.2) +
  stat_summary(fun= mean, geom="point",colour="black", size=1) +
  scale_y_continuous(breaks=c(-4,-2,0,2,4,6),limits=c(-5,6.5)) + geom_jitter(width = 0.05, alpha=0)
p
#p+scale_fill_manual(values=c("#BA1114","#F79EA4","#EBDA26","#701501","#00D9FF","#122CC4"))


#p+scale_fill_manual(values=c("Untreated"="lightgreen","ABX"="mediumpurple1","IQ146"="red","IQ036"="orange","LH115"="pink","LH043"="navy","IQ147"="royalblue","IQ133"="lightblue"))+ 
# facet_grid(~fct_relevel(Group2,'Control','PFO+','PFO-'), scales= "free")+
p+scale_fill_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","PFO+"="Red2","PFO-"="Grey"))+
  ylab("Mean Weight Changes (%)")+theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))

## Weight Change in total day 0 - day 6 (%)
Data <- read.csv("WeightChangeTotal.csv", header = TRUE, stringsAsFactors = TRUE)
head(Data)
p <- 
  Data %>%
  # mutate(Group = fct_relevel(Group, "Untreated","ABX","IQ146","IQ036","LH115","IQ147","IQ133","LH043")) %>% ggplot(aes(x=Group, y=Weight, fill=Group))+
  mutate(Group = fct_relevel(Group, "Control","ABX","pfoA+","pfoA-")) %>% ggplot(aes(x=Group, y=value, fill=Group)) + 
  geom_boxplot(outlier.colour = "white", outlier.shape = NA, outlier.alpha = 0, notch=FALSE,varwidth = FALSE, size=0.2) +
  stat_summary(fun= mean, geom="point",colour="black", size=1) +
  scale_y_continuous(breaks=c(-6,-4,-2,0,2,4,6,8,10,12),limits=c(-6,12)) + geom_jitter(width = 0.05, alpha=0)
p
#p+scale_fill_manual(values=c("#BA1114","#F79EA4","#EBDA26","#701501","#00D9FF","#122CC4"))


#p+scale_fill_manual(values=c("Untreated"="lightgreen","ABX"="mediumpurple1","IQ146"="red","IQ036"="orange","LH115"="pink","LH043"="navy","IQ147"="royalblue","IQ133"="lightblue"))+ 
# facet_grid(~fct_relevel(Group2,'Control','PFO+','PFO-'), scales= "free")+
p+scale_fill_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","pfoA+"="Red2","pfoA-"="Grey"))+
  ylab("")+ xlab("")+theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))

# Any difference in stats?
shapiro.test(Data$value)
# if no difference to normal distribution, then do aov (anova) then tukey post hoc
res.aov <- aov(value ~ Group, data = Data)
summary(res.aov)
TukeyHSD(res.aov)
# if not different from normal distribution (P<0.05) then use ANOVA.
#Summarize(value~ Group, data = Data)
#compare_means(value ~ Group,data=Data, method="anova", paired=FALSE,p.adjust.method = "none")
kruskal.test(value ~ Group, data=Data)
dunn_test(data=Data, value ~ Group,p.adjust.method = "none", detailed = FALSE)
