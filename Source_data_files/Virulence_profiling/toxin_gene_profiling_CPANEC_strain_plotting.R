# Do a heatmap for CPA-NEC strains - to show they carry pfoA genes and other toxin gens
setwd("~/")
data <- read.csv("toxin_gene_profiling_CPANEC_strain.csv")

data$group<-cut(data$Value,
                 breaks = c(-2,0.5,0.8,1))

# apply theme
library('cowplot')
theme_set(theme_cowplot())

TGP <-c("Yes"="#e5484d","No"="White")

ggplot(data=data) +
  geom_tile(aes(x=Strain, y=Toxin,fill=group),colour="white",size=4) +
  scale_fill_manual(breaks = levels(data$group),
                    values = c("white","white","#e5484d"))+
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))
