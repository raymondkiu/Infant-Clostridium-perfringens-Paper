setwd("~/")
library(tidyverse)
library(reshape2) # for melt function
library(Rmisc) # for summarySE

## Weight Change in total day 0 - day 6 (%)
Data <- read.csv("weight_change_total_data.csv", header = TRUE, stringsAsFactors = TRUE)
head(Data)
p <- 
  Data %>%
  mutate(Group = fct_relevel(Group, "Control","ABX","pfoA+","pfoA-")) %>% ggplot(aes(x=Group, y=value, fill=Group)) + 
  geom_boxplot(outlier.colour = "white", outlier.shape = NA, outlier.alpha = 0, notch=FALSE,varwidth = FALSE, size=0.2) +
  stat_summary(fun= mean, geom="point",colour="black", size=1) +
  scale_y_continuous(breaks=c(-6,-4,-2,0,2,4,6,8,10,12),limits=c(-6,12)) + geom_jitter(width = 0.05, alpha=0)
p

p+scale_fill_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","pfoA+"="Red2","pfoA-"="Grey"))+
  ylab("")+ xlab("")+theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))

# Any difference in stats?
shapiro.test(Data$value)
# if no difference to normal distribution, then do aov (anova) then tukey post hoc
res.aov <- aov(value ~ Group, data = Data)
summary(res.aov)
TukeyHSD(res.aov)
