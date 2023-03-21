setwd("~/")
library(tidyverse)
library(ggpubr)


#Time	Group	PFO	CFU
#Day 1	IQ146	pfoA+	1
#Day 1	IQ146	pfoA+	20000
#Day 1	IQ146	pfoA+	1
#Day 1	IQ146	pfoA+	1

data <- read.csv("CFU_for_plotting", header = TRUE, stringsAsFactors = TRUE)
head(data)

# Use theme cowplot - minimalist
library('cowplot')
theme_set(theme_cowplot())

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

p +scale_colour_manual(values=c("IQ146"="red","IQ129"="orange","LH115"="pink","IQ147"="navy","IQ133"="royalblue","LH043"="lightblue")) + 
  theme(axis.line = element_line(colour = 'black', size = 0.5),axis.text.x = element_text(angle = 0, vjust=1, hjust=1, size=11), axis.text.y= element_text(size=11)) + 
  ylab("")+ xlab("Days Post-challenge")+
  facet_grid(~Group, scales="free")+theme(axis.text.x = element_blank(),axis.ticks.x=element_blank())
