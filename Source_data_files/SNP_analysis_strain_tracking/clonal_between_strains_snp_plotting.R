setwd("~/")
library(tidyverse)
library(ggpubr)

# Set theme
library('cowplot')
theme_set(theme_cowplot())

data <- read.csv("clonal_between_strains_snp.csv", header = TRUE, stringsAsFactors = TRUE)
head(data)


# Basic box plot
library('cowplot')
theme_set(theme_cowplot())

p <- 
  data %>%
#  mutate(Lineage = fct_relevel(Lineage, "I", "II","III","IV","V","VI","VII","VIII","BL")) %>% 
  ggplot(aes(x=Group, y=SNP, fill=Group)) + xlab("")+
  geom_boxplot(outlier.colour = "white", outlier.shape = NA, outlier.alpha = 1, notch=FALSE,varwidth = FALSE, size=0.2) +
  scale_y_continuous(breaks=c(0,250,500,750,1000,1250,1500), limits=c(-10,1500))+
 # geom_jitter(width = 0.5, alpha=0.2) +
  scale_x_discrete(labels=c("Clonal"="Strains","NC"="Between Strains"))
p

p+scale_fill_manual(values=c("Grey","mediumpurple1"))+ theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))

