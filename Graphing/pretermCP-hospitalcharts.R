setwd("~/Desktop/pretermCP/Analysis-Rev1/Figure3/")
library(tidyverse)
library(ggpubr)

# plot bar chart first then convert - this is the usual way
HospitalPalette <- c("QCCH"="#f2ab27",
                        "SMH"="#f2e827",
                        "NNUH"="#6a13a1",
                        "RH"="#de27f2",
                        "NUTH"="#323835"
)
YearPallette <- c("2011"="#e6f5ff",
                 "2012"="#80ccff",
                 "2014"="#1aa3ff",
                 "2015"="#006bb3",
                 "2016"="#002e4d")

# Set theme
library('cowplot')
theme_set(theme_cowplot())

data <- read.csv("Hospital-stats.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)


bc<- ggplot(data, aes(x=Hospital, y=Value, fill=factor(Year)))+
  scale_fill_manual(values=YearPallette)+
  geom_bar(width = 1, stat = "identity")
#  theme(axis.text.x=element_blank(),axis.line.x = element_blank(),axis.line.y=element_blank(),axis.ticks=element_blank())+
#  ylab("") + xlab("")

bc

# plot bar charts
# Reorder bar
data$Hospital <- factor(data$Hospital, levels=c("QCCH", "SMH", "NNUH","RH","NUTH"))


bc2 <- ggplot(data, aes(x=Hospital, y=Value, fill=factor(Year)),position_stack(reverse = TRUE))+
  scale_fill_manual(values =YearPallette)+
  geom_bar(stat = "identity")+
  ylab("") + xlab("")+
  scale_y_continuous(breaks=c(0,25,50,75,100,125,150), limits=c(0,150)) +
  scale_x_discrete(labels=c("QCCH" = "A", "SMH" = "B",
                            "NNUH" = "C","RH"="D","NUTH"="E"))
  #         axis.ticks.x=element_blank())
bc2

# Plot no. of infants with longitudinal sampling points
data <- read.csv("LongitudinalSamples.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)

data$Hospital <- factor(data$Hospital, levels=c("QCCH", "SMH", "NNUH"))


bc3 <- ggplot(data, aes(x=Hospital, y=Value, fill=Hospital),position_stack(reverse = TRUE))+
  scale_fill_manual(values =HospitalPalette)+
  geom_bar(stat = "identity")+
  ylab("") + xlab("")+
  scale_y_continuous(breaks=c(0,5,10,15,20,25), limits=c(0,25)) +
  scale_x_discrete(labels=c("QCCH" = "A", "SMH" = "B",
                            "NNUH" = "C"))
#  theme(axis.text.x=element_blank())
#         axis.ticks.x=element_blank())
bc3

