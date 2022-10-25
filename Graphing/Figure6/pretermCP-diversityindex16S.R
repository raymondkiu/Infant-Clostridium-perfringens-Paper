# this script can analyse population diversity - inverse Simpson index, Shannon Weaver index and Fisher Alpha index using diversity package

library(vegan)
setwd("~/Desktop/pretermCP/Analysis-Rev1/Invivo16S/")


# Data format:
#Genus	C1	C2	C3	C4	C5	E1	E2	E3	E4	E5
#Mucispirillum	0	0	0	0	0.284577933	0	0	0	0.097360774	0.023813304
#Bacteroides	32.28779718	52.80707855	25.98486069	68.13351813	59.58650483	64.56351459	53.25338313	57.8253519	54.20298972	26.26318752
#Odoribacter	0	0	0	0	0	0	0	0	0	0
#Prevotella	0.29717682	1.37166261	0.94057014	2.065142065	0.860591099	0.122601884	0.020614056	0.218452596	0.136305084	0.040410455


X <- read.csv("Genus-diversity-index.csv",row.names=1)
head(X)
row.names(X)
Y <- t(X)


alpha <-fisher.alpha(Y, MARGIN =1, se = TRUE)

########## compute the diversity analysis

simp <- diversity(Y, "simpson")
invsimp <- diversity(Y, "invsimpson")
shannon <- diversity(Y,"shannon")


############ print the diversity index

rbind(invsimp)
rbind(shannon)
rbind(alpha)

# show all indices in column
cbind(invsimp,shannon,alpha)

Z <- cbind(invsimp,shannon)
Z
write.csv(Z, file="Diversity-index.csv") # write into csv

#Sample	1/Simpson	Shannon
#C1	2.065260335	0.932623078
#C2	2.373586835	1.088605054
#C3	1.98979124	0.95799131

## Plot a boxplot

data <- read.csv("Diversity-index-v2.0.csv", header = TRUE, stringsAsFactors = TRUE)
data
p <- 
  data %>%
  mutate(Group = fct_relevel(Group,"Control","ABX","pfoA+","pfoA-")) %>% ggplot(aes(x=Group, y=invsimp, fill=Group)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = NA, notch=FALSE,varwidth = FALSE, size=0.2, width=0.4) +
#  scale_y_log10(limits = c(1,1e9),breaks =c(1,10,100,1000,10000,100000,1000000,10000000,100000000,1000000000),
 #               label = c("0","1","2","3", "4","5","6","7","8","9")) +
 # geom_jitter(cex=0.5)
 scale_y_continuous(breaks=c(1,2,3,4,5), limits=c(1,5))

p 
#p + geom_dotplot(binaxis='y', stackdir='center',trim=TRUE,dotsize = 0.6,
#                position=position_dodge(0.5))+
p + scale_fill_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","pfoA+"="Red2","pfoA-"="Grey")) + 
  #  scale_colour_manual(values=c("pfoA+"="Red2","pfoA-"="Grey"))+
  geom_boxplot(outlier.colour = "black", outlier.shape = NA, outlier.alpha = NA, notch=FALSE,varwidth = FALSE, size=0.2, width=0.8) +
  theme(axis.line = element_line(colour = 'black', size = 0.5),axis.text.x = element_text(angle = 45, vjust=1, hjust=1, size=11), axis.text.y= element_text(size=11)) + 
  ylab("")+ xlab("")+
  facet_grid(~Day, scales="free") 
#theme(axis.text.x = element_blank(),axis.ticks.x=element_blank())
# scale_x_discrete(labels=c("Day 1" = "1", "Day 2" = "2", "Day 3" ="3", "Day 4"="4","Day 5"="5","Day 6"="6", "Day 7"="7"))+


# Stats
Data <- read.csv("Diversity-index-v2.0.csv", header = TRUE, stringsAsFactors = TRUE)
Data
shapiro.test(Data$invsimp) # seems normal distribution

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



# not Kurskal Wallis as data is normally distributed
kruskal.test(invsimp ~ Group, data=Day0)
kruskal.test(invsimp ~ Group, data=Day2)
kruskal.test(invsimp ~ Group, data=Day6)


dunnTest(Day0$invsimp, Day0$Group,method="bonferroni")
dunnTest(Day2$invsimp, Day2$Group,method="bonferroni")
dunnTest(Day6$invsimp, Day6$Group,method="bonferroni")

library(rstatix)
dunn_test(Day0, invsimp ~ Group, p.adjust.method = "BH")
dunn_test(Day2, invsimp ~ Group, p.adjust.method = "BH")
dunn_test(Day6, invsimp ~ Group, p.adjust.method = "BH")
# No statistical differences found.







