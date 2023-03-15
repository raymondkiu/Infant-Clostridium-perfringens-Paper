library(vegan)
setwd("/")
data <- read.csv("taxa_genus_diversity_index.csv", header = TRUE, stringsAsFactors = TRUE)
data
p <- 
  data %>%
  mutate(Group = fct_relevel(Group,"Control","ABX","pfoA+","pfoA-")) %>% ggplot(aes(x=Group, y=invsimp, fill=Group)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = NA, notch=FALSE,varwidth = FALSE, size=0.2, width=0.4) +
  scale_y_continuous(breaks=c(1,2,3,4,5), limits=c(1,5))

p 

p + scale_fill_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","pfoA+"="Red2","pfoA-"="Grey")) + 
  #  scale_colour_manual(values=c("pfoA+"="Red2","pfoA-"="Grey"))+
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = NA, notch=FALSE,varwidth = FALSE, size=0.2, width=0.8) +
  theme(axis.line = element_line(colour = 'black', size = 0.5),axis.text.x = element_text(angle = 45, vjust=1, hjust=1, size=11), axis.text.y= element_text(size=11)) + 
  ylab("")+ xlab("")+
  facet_grid(~Day, scales="free") 

# Stats
Data <- read.csv("taxa_genus_diversity_index.csv", header = TRUE, stringsAsFactors = TRUE)
Data
shapiro.test(Data$invsimp) # seems like a normal distribution

library(FSA)
Summarize(invsimp~ Group, data = Data)

Day0 <- filter(Data, Day== "Day 0")
Day2 <- filter(Data, Day== "Day 2")
Day6 <- filter(Data, Day== "Day 6")

shapiro.test(Day0$invsimp)
shapiro.test(Day2$invsimp)
shapiro.test(Day6$invsimp)

# run ANOVA
res.aov0 <- aov(invsimp ~ Group, data = Day0)
res.aov2 <- aov(invsimp ~ Group, data = Day2)
res.aov6 <- aov(invsimp ~ Group, data = Day6)
summary(res.aov0)
summary(res.aov2)
summary(res.aov6)
TukeyHSD(res.aov0)
TukeyHSD(res.aov2)
TukeyHSD(res.aov6)
