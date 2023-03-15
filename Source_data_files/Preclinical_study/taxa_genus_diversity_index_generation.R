library(vegan)
setwd("~/")

X <- read.csv("taxa_genus_raw_count.csv",row.names=1)
head(X)
row.names(X)
Y <- t(X)

########## compute the diversity analysis

invsimp <- diversity(Y, "invsimpson")
shannon <- diversity(Y,"shannon")


############ print the diversity index

rbind(invsimp)
rbind(shannon)

# show all indices in column
cbind(invsimp,shannon,alpha)

Z <- cbind(invsimp,shannon)
Z
