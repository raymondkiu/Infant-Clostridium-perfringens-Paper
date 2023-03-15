setwd("~/Desktop/pretermCP/Analysis-Rev1/Figure3/")
library(tidyverse)
library(ggpubr)

# plot bar chart first then convert - this is the usual way
HospitalPalette <- c("A"="#f2ab27",
                        "B"="#f2e827",
                        "C"="#6a13a1",
                        "D"="#de27f2",
                        "E"="#323835"
)

data <- read.csv("longitudinal_strain_hospital.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)

bc3 <- ggplot(data, aes(x=Hospital, y=Value, fill=Hospital),position_stack(reverse = TRUE))+
  scale_fill_manual(values =HospitalPalette)+
  geom_bar(stat = "identity")+
  ylab("") + xlab("")+
  scale_y_continuous(breaks=c(0,5,10,15,20,25), limits=c(0,25)) 
#  theme(axis.text.x=element_blank())
#         axis.ticks.x=element_blank())
bc3
