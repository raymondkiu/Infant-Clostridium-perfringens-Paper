install.packages("vegan")
library(vegan)

setwd("~/")
pc = read.csv(file = "file.csv", header= TRUE)
#make community matrix - extract columns with gene information from 4th column
com = pc[,3:ncol(pc)]
#turn gene data frame into a matrix
m_com = as.matrix(com)
set.seed(123)
nmds = metaMDS(m_com, k=4, try = 30, autotransform=TRUE, distance = "jaccard")
# stress value < 0.1 is considered OK and confident to infer the plot
plot(nmds)

ordiplot(nmds, type="text")
data.envfit <- envfit(nmds)
plot(data.envfit)
goodness(nmds)
stressplot(nmds)

#extract NMDS scores (x and y coordinates)
data.scores = as.data.frame(scores(nmds))

#add columns to data frame 
data.scores$Toxinotype = pc$Toxinotype
data.scores$Cluster = pc$Cluster
head(data.scores)

library(ggplot2)

xx = ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size =2, aes( colour = Cluster))+ 
  theme(axis.text.y = element_text(colour = "black", size = 12), 
        axis.text.x = element_text(colour = "black", size = 12), 
        legend.text = element_text(size = 10, colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold",size = 10), 
        axis.title.x = element_text(face = "bold",size = 10, colour = "black"), 
        legend.title = element_text(size = 10, colour = "black"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Cluster", y = "NMDS2")  + 
  scale_colour_manual(values = c("#BA1114","#F79EA4","#EBDA26","#701501","#00D9FF","#122CC4")) 

xx
