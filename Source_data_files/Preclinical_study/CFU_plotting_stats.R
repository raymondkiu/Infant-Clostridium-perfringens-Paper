setwd("~/")
library(tidyverse)
library(ggpubr)

# Format
#Time	Group	PFO	CFU
#Day 1	IQ146	PFO+	1
#Day 1	IQ146	PFO+	20000
#Day 1	IQ146	PFO+	1
#Day 1	IQ146	PFO+	1

data <- read.csv("CFU_for_plotting_stats.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)

# Use theme cowplot - minimalist
library('cowplot')
theme_set(theme_cowplot())

# Do PFO+ and PFO- groups
p <- 
  data %>%
  mutate(PFO = fct_relevel(PFO,"PFO+","PFO-")) %>% ggplot(aes(x=Time, y=CFU, fill=PFO)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = NA, notch=FALSE,varwidth = FALSE, size=0.2, width=0.5) +
  scale_y_log10(limits = c(1,1e9),breaks =c(1,10,100,1000,10000,100000,1000000,10000000,100000000,1000000000),
                label = c("0","1","2","3", "4","5","6","7","8","9")) +
  geom_jitter(cex=0.5)

p 

p + scale_fill_manual(values=c("PFO+"="Red2","PFO-"="Grey")) + 
  theme(axis.line = element_line(colour = 'black', size = 0.5),axis.text.x = element_text(angle = 45, vjust=1, hjust=1, size=11), axis.text.y= element_text(size=11)) + 
  ylab("")+ xlab("")+
  facet_grid(~PFO, scales="free") 

# Stats
Data <- read.csv("CFU_data_stats", header = TRUE, stringsAsFactors = TRUE)
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
