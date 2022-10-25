setwd("~/Desktop/pretermCP/Analysis-Rev1/Figure3/")
library(tidyverse)
library(ggpubr)

# Set theme
library('cowplot')
theme_set(theme_cowplot())

data <- read.csv("comparison_snp_R.csv", header = TRUE, stringsAsFactors = TRUE)
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
#p+scale_fill_manual(values=c("#BA1114","#F79EA4","#EBDA26","#701501","#00D9FF","#122CC4"))
p+scale_fill_manual(values=c("Grey","mediumpurple1"))+ theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))


## STATS
shapiro.test(data$SNP) # seem to be different from normal distribution

# Run statistical analysis

library(FSA)

Summarize(SNP~ Group, data = data)

wilcox.test(SNP ~ Group, data=data)
t.test(SNP ~ Group, data=data)
# Test which group is statistically different (if overall KW test P<0.05), suitable for unequal number of observations
dunnTest(Day1$value, Day1$Group,method="none")
dunnTest(Day2$value, Day2$Group,method="none")