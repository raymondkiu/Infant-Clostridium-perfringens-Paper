setwd("~/")
library(tidyverse)
library(ggpubr)

# Do a boxplot as requested - bar plots cannot deploy individual points as it is based on stats summary
data <- read.csv("Sporulation.csv", header = TRUE, stringsAsFactors = TRUE)
data
p <- data %>%
  mutate(Lineage = fct_relevel(Lineage, 
                               "Lineage 1","Lineage 3","Lineage 5","Lineage 6","Lineage 7")) %>% 
  ggplot(aes(x=Lineage, y=Spores, fill=Lineage)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = NA, notch=FALSE,varwidth = FALSE, size=0.2, width=0.8) +
  scale_y_log10(limits = c(-100,1e8),breaks =c(1,10,100,1000,10000,100000,1000000,10000000,100000000,1000000000),
                label = c("0","1","2","3", "4","5","6","7","8","9"))+  
  scale_fill_manual(values=c("Lineage 1"="#ba1114","Lineage 3"="#ebda26","Lineage 5"="#122cc4","Lineage 6"="#00d9ff","Lineage 7"="#4b0c71"))+
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1)) +
  xlab("") + ylab("")+
  scale_x_discrete(labels=c("Lineage 1" = "Lineage I", "Lineage 3" = "Lineage III",
                            "Lineage 5" = "Lineage V", "Lineage 6"="Lineage VI","Lineage 7"="Lineage VII"))

  
p 
