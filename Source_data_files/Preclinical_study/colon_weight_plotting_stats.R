setwd("~/")
library(ggplot2)
library(tidyverse)
library(ggpubr)

data <- read.csv("colon_weight_data.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)

# Basic box plot
library('cowplot')
theme_set(theme_cowplot())

p <- 
  data %>%
 # mutate(Group = fct_relevel(Group, "Untreated","ABX","IQ146","IQ036","LH115","IQ147","IQ133","LH043")) %>% ggplot(aes(x=Group, y=Weight, fill=Group))+
  mutate(Group = fct_relevel(Group, "Control","ABX","pfoA+","pfoA-")) %>% ggplot(aes(x=Group, y=Weight, fill=Group)) + 
  geom_boxplot(outlier.colour = "white", outlier.shape = NA, outlier.alpha = 0, notch=FALSE,varwidth = FALSE, size=0.2) +
  scale_y_continuous(limits=c(110,260)) + geom_jitter(width = 0.05, alpha=0)
p

p+scale_fill_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","pfoA+"="Red2","pfoA-"="Grey"))+
   ylab("Colon Weight (mg)")+theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))

# barplot
library('cowplot')
theme_set(theme_cowplot())
  
data$Group <- factor(data$Group, levels = c("Control", "ABX","pfoA+","pfoA-"))
library(Rmisc)
data <- summarySE(data, measurevar="Weight", groupvars=c("Group")) # Group first then Isolate
data
  
  p <- 
    ggplot(data,aes(x=Group, y=Weight, fill=Group)) + geom_bar(position="dodge", stat="identity") +
    scale_y_continuous(breaks=c(0,50,100,150,200,250), limits=c(0,250)) +
    geom_errorbar(aes(ymin=Weight-se, ymax=Weight+se),
                  width=.3,                    # Width of the error bars
                  position=position_dodge(.9))
  p+scale_fill_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","pfoA+"="Red2","pfoA-"="Grey"))+
    ylab("")+xlab("")+theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))
  
# Stats
 Data <- read.csv("colon_weight_data.csv", header = TRUE, stringsAsFactors = TRUE)
 head(Data)
  
# Any difference in stats?
  shapiro.test(Data$Weight)
  
# Different, not normal - use Kruskal wallies test
  
library(FSA)
Summarize(Weight~ Group, data = Data)
  
res.kruskal <- kruskal.test(Weight ~ Group, data=Data)
res.kruskal
  
# Test which group is statistically different, suitable for unequal number of observations
dunn_test(Data,Weight ~ Group,p.adjust.method="BH")
