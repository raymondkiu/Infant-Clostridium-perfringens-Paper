setwd("~/Desktop/pretermCP/Analysis-Rev1/Survivalassays/")
library(tidyverse)
library(ggpubr)
#library(scales)

# Data format in csv
# 
# Sample	Isolate	Group	Value
# Sample	Isolate	Group	Value
# Q215	IQ146	Bell Stage II/III	200
# Q215	IQ146	Bell Stage II/III	100
# Q215	IQ146	Bell Stage II/III	200
# Q088	IQ036	Bell Stage II/III	200
# Q088	IQ036	Bell Stage II/III	150
# Q088	IQ036	Bell Stage II/III	50
# Q143	IQ129	Bell Stage II/III	1400000


#data <- read.csv("dotplot-data.csv",sep =";", header = TRUE, stringsAsFactors = FALSE)
data <- read.csv("Sporulation.csv", header = TRUE, stringsAsFactors = TRUE)
data

# Basic box plot
library('cowplot')
theme_set(theme_cowplot())

# Summary - make a statistical summary for se,sd and ci for error bars before plotting
#install.packages('Rmisc', dependencies = TRUE)
library(Rmisc)
data <- summarySE(data, measurevar="Spores", groupvars=c("Lineage")) # Group first then Isolate
data

# Reorder only group
data$Lineage <- factor(data$Lineage, levels = c("Lineage 1","Lineage 2","Lineage 3","Lineage 4","Lineage 5","Lineage 6","Lineage 7","Lineage 8"))
data  # Check if they look great
# Reorder labeling using fct_relevel
p <- data %>%
  mutate(Lineage = fct_relevel(Lineage, 
                               "Lineage 1","Lineage 3","Lineage 5","Lineage 6","Lineage 7")) %>% 
  ggplot(aes(x=Lineage, y=Spores, fill=Lineage)) + geom_bar(position="dodge", stat="identity") +
 # scale_y_continuous(limits=c(-100,1e8)) +
  geom_errorbar(aes(ymin=Spores-se, ymax=Spores+se),
                width=.3,                    # Width of the error bars
                position=position_dodge(.9),color="black")
p # not good, need Log10 scale

# use log10 scale + change colour schemes
#library(scales) # for trans_breaks and trans_format
p+ scale_y_log10(limits = c(-100,1e8),breaks =c(1,10,100,1000,10000,100000,1000000,10000000,100000000,1000000000),
                 label = c("0","1","2","3", "4","5","6","7","8","9"))+  
#  scale_y_log10(limits = c(-100,1e8),breaks = trans_breaks("log10", function(x) 10^x),
 #                labels = trans_format("log10", math_format(10^.x)))+
  scale_fill_manual(values=c("Lineage 1"="#ba1114","Lineage 3"="#ebda26","Lineage 5"="#122cc4","Lineage 6"="#00d9ff","Lineage 7"="#4b0c71"))+
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1)) +
  xlab("") + ylab("")+
  scale_x_discrete(labels=c("Lineage 1" = "Lineage I", "Lineage 3" = "Lineage III",
                              "Lineage 5" = "Lineage V", "Lineage 6"="Lineage VI","Lineage 7"="Lineage VII"))
#annotation_logticks(sides="l",long = unit(0.1, "cm"),  colour = "black",size = 0.2) 
                                                                                      

# stats: use Kruskal-Wallis, non-parametric on which isolate is significantly different
Data <- read.csv("Sporulation-stats.csv", header = TRUE, stringsAsFactors = TRUE)
Data

library(FSA)
Summarize(Spores~ Lineage, data = Data)
shapiro.test(Data$Spores)

res.kruskal <- kruskal.test(Spores ~ Lineage, data=Data)
res.kruskal

# Test which group is statistically different, suitable for unequal number of observations
dunn_test(data, Spores ~ Lineage, p.adjust.method = "holm")



