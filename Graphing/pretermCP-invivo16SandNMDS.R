setwd("~/Desktop/pretermCP/Analysis-Rev1/Invivo16S/")
library(tidyverse)

# Data format:
# Group	Individual	Sample	Mucispirillum	Bacteroides	Odoribacter	Prevotella	Bilophila	Desulfovibrio	Staphylococcus	Enterococcus	Lactobacillus
# ABX	Day 1	ABX1M1	0	0.774499136	0	0	0	0	0	90.03392434	9.191576522
# ABX	Day 1	ABX1M2	0	0.016104788	0	0	0	0	0	97.02061413	2.963281082
# ABX	Day 1	ABX1M3	0	1.151491966	0	0	0	0	0	67.44835501	31.38102525
# ABX	Day 2	ABX2M1	0	0.02336667	0	0	0	0	0	99.82708664	0.149546687
# ABX	Day 2	ABX2M2	0	0.045372051	0	0	0	0	0	98.25771325	1.696914701
# ABX	Day 2	ABX2M3	0	0.014578322	0	0	0	0	0	54.90196078	45.07252715
# ABX	Day 3	ABX3M1	0	41.47293945	0	0	0	0	0	36.32270514	22.08949738
# ABX	Day 3	ABX3M2	0	38.88390569	0	0	0	0	0	37.05791902	24.04536135
# ABX	Day 3	ABX3M3	0	0.036656891	0	0	0	0	0	22.66728872	77.29605439
# ABX	Day 4	ABX4M1	0	58.17130199	0	0	0	0	0	40.11403725	1.677742227
# ABX	Day 4	ABX4M2	0	32.29964068	0	0	0	0	0	61.45914701	6.241212311
# ABX	Day 4	ABX4M3	0	9.003062266	0	0	0	0	0	84.58659408	6.410343654
# ABX	Day 5	ABX5M1	0	56.85149923	0	0	0	0	0	23.11133543	19.99816467
# ABX	Day 5	ABX5M2	0	69.03542783	0	0	0	0	0	27.2353272	3.717406103
# ABX	Day 5	ABX5M3	0	31.60314736	0	0	0	0	0	28.368358	40.01554253
# ABX	Day 6	ABX6M1	0	85.39698003	0	0	0	0	0	11.0058451	3.231855821
# ABX	Day 6	ABX6M2	0	77.86970535	0	0	0	0	0	14.47351724	7.608557724
# ABX	Day 6	ABX6M3	0	56.48044908	0	0	0	0	0	12.96089815	30.43369603

# Barplot for 16S visualisation
data <- read.csv("Taxa-mean-PFOonly.csv", header = TRUE, stringsAsFactors = TRUE)
data

# melt data
#plotDat <- reshape::melt(data)
#plotDat
#mdata <- melt(mydata, id=c("id","time"))
plotDat <- reshape::melt(data, id=c("Group","Day"))
head(plotDat)
# Basic box plot
library('cowplot')
theme_set(theme_cowplot())

library(Rmisc)
plotDat<- summarySE(plotDat, measurevar="value", groupvars=c("Group","Day","variable")) # Group first then Isolate
plotDat

# reorder
#plotDat$Group <- factor(plotDat$Group, levels = c("Control", "ABX","IQ146","IQ129","LH115","IQ147","IQ133","LH043"))
plotDat$Group <- factor(plotDat$Group, levels = c("Control", "ABX","PFO+","PFO-"))
p <- 
  ggplot(plotDat,aes(x=Day, y=value,fill=variable)) +
  geom_bar(stat='identity',position='stack') +
  scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(-5,104))
p

# Genuspalette define color - the only way
genusPalette <- c( Mucispirillum="tan3",
                  Bacteroides="lightpink",
                  Bacillus="lightyellow",
                  Prevotella="hotpink",
                  Jeotgalicoccus="dodgerblue",
                  Desulfovibrio="navy",
                  Staphylococcus="orange",
                  Enterococcus="gold",
                  Lactobacillus="slateblue3",
                  Streptococcus="purple",
                  Candidatus.Arthromitus="lightblue",
                  Clostridium="red1",
                  Blautia="darkgreen",
                  Coprococcus="brown4",
                  Dorea="mediumpurple1",
                  Dehalobacterium="palegreen",
                  Anaerotruncus="sienna1",
                  Oscillospira="royalblue",
                  Ruminococcus="yellowgreen",
                  Allobaculum="tomato1",
                  Coprobacillus="darkorange",
                  Anaeroplasma="cyan")

p + scale_fill_manual(values = genusPalette) +
facet_grid( ~ Group, scales="free") + 
theme(
  axis.text.x = element_text(angle = 45, vjust=1, hjust=1),
     axis.title.x=element_blank())
   #  axis.text.x=element_blank(),
  #   axis.ticks.x=element_blank())

# NMDS
library(vegan)
data <- read.csv("R-Taxa.csv", header = TRUE, stringsAsFactors = TRUE)
data
#make community matrix - extract columns with gene information from 4th column (col 1 and 2 are metadata)
com = data[,6:ncol(data)]
com
#turn gene data frame into a matrix
m_com = as.matrix(com)
set.seed(123)
nmds = metaMDS(m_com, k=7, try = 50, autotransform=TRUE, distance = "bray")
# stress value < 0.1 is considered OK and confident to infer the plot
nmds
plot(nmds)

ordiplot(nmds, type="text")

goodness(nmds)
stressplot(nmds)

#extract NMDS scores (x and y coordinates)
data.scores = as.data.frame(scores(nmds))

#add columns to data frame (must do in order to visualise using geom point)
data.scores$Day = data$Day
data.scores$Group = data$Group
data.scores$PFO = data$PFO
data.scores

# Reorder
data.scores$Group<- factor(data.scores$Group, levels = c("Control", "ABX", "CP","IQ146","IQ129","LH115","IQ147","IQ133","LH043"))
data.scores
library(ggplot2)

# Grouped by strains and day

xx = ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size =2, aes(colour =Group, shape=Day))+ 
  #geom_text(aes(label=Mouse),hjust=0, vjust=0)+
  theme_bw()+  # Use bw theme better for NMDS plot and remove all the background + grids
  theme(axis.text.y = element_text(colour = "black", size = 12), 
        axis.text.x = element_text(colour = "black", size = 12), 
        legend.text = element_text(size = 10, colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold",size = 10), 
        axis.title.x = element_text(face = "bold",size = 10, colour = "black"), 
        legend.title = element_text(size = 10, colour = "black"),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.8),
        legend.key=element_blank()) + 
        coord_equal() +
  labs(x = "NMDS1", colour = "Group", y = "NMDS2")+ 
  scale_colour_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","IQ146"="red","IQ129"="orange","LH115"="pink","IQ147"="navy","IQ133"="royalblue","LH043"="lightblue"))

#  scale_colour_manual(values = c("#BA1114","#F79EA4","#EBDA26","#701501","#00D9FF","#122CC4")) 
 # scale_color_manual(values=c("lightgreen","mediumpurple1","red")) 
xx 

# Grouped by PFO and Day

yy = ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size =2, aes(colour =PFO, shape=Day))+ 
  #geom_text(aes(label=Mouse),hjust=0, vjust=0)+
  theme_bw()+  # Use bw theme better for NMDS plot and remove all the background + grids
  theme(axis.text.y = element_text(colour = "black", size = 12), 
        axis.text.x = element_text(colour = "black", size = 12), 
        legend.text = element_text(size = 10, colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold",size = 10), 
        axis.title.x = element_text(face = "bold",size = 10, colour = "black"), 
        legend.title = element_text(size = 10, colour = "black"),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.8),
        legend.key=element_blank()) + 
  coord_equal() +
  labs(x = "NMDS1", colour = "Group", y = "NMDS2")+ 
  scale_colour_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","PFO+"="Red2","PFO-"="Grey"))
yy

# Ordination using envfit
en = envfit(nmds, com, permutations = 999, na.rm = TRUE)
en  # to check what it is like
# Continuous variables will be vectors, categorical variables will be factors
# To plot ordination onto NMDS
data.scores = as.data.frame(scores(nmds))
data.scores$Group = data$Group
en_coord_cont = as.data.frame(scores(en, "vectors")) * ordiArrowMul(en)
en_coord_cont


# ggplot2 (modified from above)
xx = ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size =2, aes(colour =Group))+ 
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               data = en_coord_cont,  # See available arrow types in example above
               arrow = arrow(
                 length = unit(0.01, "npc"), 
                 type="open" # Describes arrow head (open or closed)
               ),size =0.5, alpha = 0.5, colour = "blue") +
  geom_text(data = en_coord_cont, aes(x = NMDS1, y = NMDS2), 
            size=3,label = row.names(en_coord_cont)) + 
  #geom_text(aes(label=Mouse),hjust=0, vjust=0)+
  theme(axis.text.y = element_text(colour = "black", size = 12), 
        axis.text.x = element_text(colour = "black", size = 12), 
        legend.text = element_text(size = 10, colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold",size = 10), 
        axis.title.x = element_text(face = "bold",size = 10, colour = "black"), 
        legend.title = element_text(size = 10, colour = "black"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Group",y= "NMDS2")  + 
  scale_colour_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","IQ146"="red","IQ129"="orange","LH115"="pink","IQ147"="navy","IQ133"="royalblue","LH043"="lightblue"))

xx



