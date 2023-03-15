setwd("~/")
library(tidyverse)
library(reshape2) # for melt function
library(Rmisc) # for summarySE
data <- read.csv("weight_change_day_data.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)

#data1 <- melt(data,  id.vars ="Group" ,  variable.name ="Day")
#data1
#write.csv(data1,"weight-change-melted.csv")
# Input format
# Time	Group	Value
# Day 0	CP	0
# Day 0	CP	0

# Use theme cowplot - minimalist
library('cowplot')
theme_set(theme_cowplot())

# Prep stats for error bar
data <- summarySE(data, measurevar="value", groupvars=c("Group","Day"))
data

p <- data %>% mutate(Group = fct_relevel(Group, "Control","ABX","pfoA+","pfoA-")) %>%
  ggplot(aes(x=Day, y=value, color=Group, group=Group)) +
  geom_line(size=0.6)+
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
