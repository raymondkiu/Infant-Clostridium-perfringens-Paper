# NMDS
library(vegan)
data <- read.csv("taxa_data_nmds.csv", header = TRUE, stringsAsFactors = TRUE)
data
#make community matrix - extract columns with gene information from 4th column (col 1 and 2 are metadata)
com = data[,6:ncol(data)]
com
#turn gene data frame into a matrix
m_com = as.matrix(com)
set.seed(123)

nmds = metaMDS(m_com, k=3, try = 50, autotransform=TRUE, distance = "bray")
# stress value < 0.1 is considered OK and confident to infer the plot
nmds
plot(nmds)

ordiplot(nmds, type="text")

goodness(nmds)
stressplot(nmds)

#extract NMDS scores (x and y coordinates)
data.scores=as.data.frame(scores(nmds)$sites) # latest way to force scores of nmds object into a data frame

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
  scale_colour_manual(values=c("Control"="lightgreen","ABX"="mediumpurple1","pfoA+"="Red2","pfoA-"="Grey"))
yy
