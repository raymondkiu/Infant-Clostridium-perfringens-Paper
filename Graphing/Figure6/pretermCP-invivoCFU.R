setwd("~/Desktop/pretermCP/Analysis-Rev1/InvivoHistology/")
library(tidyverse)
library(ggpubr)


#Time	Group	PFO	CFU
#Day 1	IQ146	PFO+	1
#Day 1	IQ146	PFO+	20000
#Day 1	IQ146	PFO+	1
#Day 1	IQ146	PFO+	1

data <- read.csv("CFU-invivo.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)

# Use theme cowplot - minimalist
library('cowplot')
theme_set(theme_cowplot())

# Build a boxplot (no need to do extra stats with summarySE function (Rmisc)):
# Reorder the groups

# define colour

p <- 
  data %>%
  mutate(Group = fct_relevel(Group, 
                             "IQ146", "IQ129", 
                             "LH115","IQ147","IQ133", "LH043")) %>% ggplot(aes(x=Time, y=CFU, colour=Group)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = NA, notch=FALSE,varwidth = FALSE, size=0.2, width=1) +
  scale_y_log10(limits = c(100,1e8),breaks =c(100,1000,10000,100000,1000000,10000000,100000000),
                label = c("2","3", "4","5","6","7","8"))
  #geom_jitter(position=position_jitter(0.5), cex=0.6)

p 
#p + geom_dotplot(binaxis='y', stackdir='center',trim=TRUE,dotsize = 0.6,
#                position=position_dodge(0.5))+
p +scale_colour_manual(values=c("IQ146"="red","IQ129"="orange","LH115"="pink","IQ147"="navy","IQ133"="royalblue","LH043"="lightblue")) + 
  theme(axis.line = element_line(colour = 'black', size = 0.5),axis.text.x = element_text(angle = 0, vjust=1, hjust=1, size=11), axis.text.y= element_text(size=11)) + 
  ylab("")+ xlab("Days Post-challenge")+
  facet_grid(~Group, scales="free")+theme(axis.text.x = element_blank(),axis.ticks.x=element_blank())
 # scale_x_discrete(labels=c("Day 1" = "1", "Day 2" = "2", "Day 3" ="3", "Day 4"="4","Day 5"="5","Day 6"="6", "Day 7"="7"))
#axis.text.x = element_text(angle = 45, vjust=1, hjust=1)

# Do PFO+ and PFO- groups
p <- 
  data %>%
  mutate(PFO = fct_relevel(PFO,"PFO+","PFO-")) %>% ggplot(aes(x=Time, y=CFU, fill=PFO)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = NA, notch=FALSE,varwidth = FALSE, size=0.2, width=0.5) +
  scale_y_log10(limits = c(1,1e9),breaks =c(1,10,100,1000,10000,100000,1000000,10000000,100000000,1000000000),
                label = c("0","1","2","3", "4","5","6","7","8","9")) +
  geom_jitter(cex=0.5)

p 
#p + geom_dotplot(binaxis='y', stackdir='center',trim=TRUE,dotsize = 0.6,
#                position=position_dodge(0.5))+
p + scale_fill_manual(values=c("PFO+"="Red2","PFO-"="Grey")) + 
#  scale_colour_manual(values=c("PFO+"="Red2","PFO-"="Grey"))+
  theme(axis.line = element_line(colour = 'black', size = 0.5),axis.text.x = element_text(angle = 45, vjust=1, hjust=1, size=11), axis.text.y= element_text(size=11)) + 
  ylab("")+ xlab("")+
  facet_grid(~PFO, scales="free") 
#theme(axis.text.x = element_blank(),axis.ticks.x=element_blank())
 # scale_x_discrete(labels=c("Day 1" = "1", "Day 2" = "2", "Day 3" ="3", "Day 4"="4","Day 5"="5","Day 6"="6", "Day 7"="7"))+




#########
# Stats #
#########

# Stats
Data <- read.csv("CFU-invivo-stats.csv", header = TRUE, stringsAsFactors = TRUE)
head(Data)

shapiro.test(Data$CFU) # seems like not normally distributed

library(FSA)
Summarize(CFU~ Time, data = Data)

Day0 <- filter(Data, Time== "Day 0")
Day1 <- filter(Data, Time== "Day 1")
Day2 <- filter(Data, Time== "Day 2")
Day3 <- filter(Data, Time== "Day 3")
Day4 <- filter(Data, Time== "Day 4")
Day5 <- filter(Data, Time== "Day 5")
Day6 <- filter(Data, Time== "Day 6")

library(rstatix) # for wilcox_test -> better than wilcox.test as can use dplyr filter function to select data
wilcox_test(CFU ~ PFO, data=Day0)
wilcox_test(CFU ~ PFO, data=Day1)
wilcox_test(CFU ~ PFO, data=Day2)
wilcox_test(CFU ~ PFO, data=Day3)
wilcox_test(CFU ~ PFO, data=Day4)
wilcox_test(CFU ~ PFO, data=Day5)
wilcox_test(CFU ~ PFO, data=Day6)

# No statistical differences found.


