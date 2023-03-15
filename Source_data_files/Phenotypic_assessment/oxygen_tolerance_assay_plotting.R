setwd("~/")
library(tidyverse)
library(ggpubr)

# Data format in csv
# Group	Lineage	Day	Percentage
# PFO+	Lineage 7	Day 0	100
# PFO+	Lineage 7	Day 2	9.375000023
# PFO+	Lineage 7	Day 4	2.100000005
# PFO+	Lineage 7	Day 6	0.600000002
# PFO+	Lineage 7	Day 8	0.03375
# PFO+	Lineage 7	Day 10	0.008625

# Do one line plot grouped in all lineages
data <- read.csv("oxygen_tolerance_assay_for_plotting.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)
q <- data %>% 
  ggplot(aes(x=Time, y=Value, group=Lineage, color=Lineage)) +
  scale_color_manual(values=c("Lineage 1"="#ba1114","Lineage 2"="#f79ea4","Lineage 3"="#ebda26","Lineage 4"="#701501","Lineage 5"="#122cc4","Lineage 6"="#00d9ff","Lineage 7"="#4b0c71","Lineage 8"="#daaff3"))+
  scale_x_continuous(breaks=c(0,24,48,72,96,120,144,168,192,216,240,264,288,312,336), limits=c(0,336))+
  #scale_y_log10(limits = c(1e-6,150),breaks = trans_breaks("log10", function(x) 10^x))+
  scale_y_log10(limits = c(1e-8,120),breaks =c(100,10,1,0.1,0.01,0.001,0.0001,0.00001,0.000001,0.0000001),
                label = c("100", "10","1", "0.1","0.01","0.001","0.0001","0.00001","0.000001","0"))+
  stat_summary(fun=mean, geom="line")+ # only scale_color_manual goes with stat_summary
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))
q
