setwd("~/")
library(tidyverse)
library(ggpubr)

# plot bar chart first then convert - this is the usual way
HospitalPalette <- c("A"="#f2ab27",
                        "B"="#f2e827",
                        "C"="#6a13a1",
                        "D"="#de27f2",
                        "E"="#323835"
)
YearPallette <- c("2011"="#e6f5ff",
                 "2012"="#80ccff",
                 "2014"="#1aa3ff",
                 "2015"="#006bb3",
                 "2016"="#002e4d")

# Set theme
library('cowplot')
theme_set(theme_cowplot())

data <- read.csv("hospital_isolate_stats.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)


bc<- ggplot(data, aes(x=Hospital, y=Value, fill=factor(Year)))+
  scale_fill_manual(values=YearPallette)+
  geom_bar(width = 1, stat = "identity")
#  theme(axis.text.x=element_blank(),axis.line.x = element_blank(),axis.line.y=element_blank(),axis.ticks=element_blank())+
#  ylab("") + xlab("")

bc
