setwd("~/")
library(tidyverse)

data <- read.csv("histo_crypt_length_data.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)

# Basic bar plot - not boxpplot
library('cowplot')
theme_set(theme_cowplot())

data$Group <- factor(data$Group, levels = c("Control", "ABX","pfoA+","pfoA-"))
#install.packages('Rmisc', dependencies = TRUE)
library(Rmisc)
data <- summarySE(data, measurevar="Cryptlength", groupvars=c("Group")) # Group first then Isolate
data

p <- 
  ggplot(data,aes(x=Group, y=Cryptlength, fill=Group)) + geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(breaks=c(0,50,100,150,200,250,300), limits=c(0,300)) +
  geom_errorbar(aes(ymin=Cryptlength-se, ymax=Cryptlength+se),
                width=.3,                    # Width of the error bars
                position=position_dodge(.9))
p

# Change colour for groups

p+  scale_fill_manual(values=c("Control"="lightgreen", "ABX"="mediumpurple1", "pfoA+"="red2","pfoA-"="grey")) + 
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))

# Stats
Data <- read.csv("histo_crypt_length_data.csv", header = TRUE, stringsAsFactors = TRUE)
Data

# Any difference in stats?
shapiro.test(Data$Cryptlength)
# if no difference to normal distribution, then do aov (anova) then tukey post hoc
# if different do non-parametric

library(FSA)
Summarize(Cryptlength~ Group, data = Data)

res.kruskal <- kruskal.test(Cryptlength ~ Group, data=Data)
res.kruskal

# Test which group is statistically different, suitable for unequal number of observations

library(rstatix)
dunn_test(Data, Cryptlength ~ Group, p.adjust.method = "BH")
