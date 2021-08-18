library(rhierbaps)
library(ggtree)
library(phytools)

# Set seed
set.seed((0) # To enable reproduction of the same result

# Set working directory (path to your file)
setwd("")

# Load FASTA file  
snp.matrix <- load_fasta("core_gene_alignment.aln.snp_sites.aln")

# Predict clustering (main run)
hb.results <- hierBAPS(snp.matrix, max.depth = 2, n.pops = 20, quiet =F,n.cores = 4)
# n.cores only available in Linux R not in R studio

# Look into your data, first column should be your isolate names
head(hb.results$partition.df)
tail(hb.results$partition.df)

############### this section is additional and not tested ###########################
hb.results <- hierBAPS(snp.matrix, max.depth = 2,  n.extra.rounds = Inf, 
                       quiet = TRUE)

system.time(hierBAPS(snp.matrix, max.depth = 2, n.pops = 5, quiet = TRUE))
#>    user  system elapsed 
#>  83.378  10.177  96.571
#check running time

# plotting:

tree1 <-read.tree("cfml-new.output.labelled_tree.newick")
iqtree <- midpoint.root(tree1)
plotTree(tree1)
plotTree(iqtree)
gg <- ggtree(iqtree, layout = "circular")
gg <- gg %<+% hb.results$partition.df
gg <- gg + geom_tippoint(aes(color = factor(`level 1`)))
gg

plot_sub_cluster(hb.results, iqtree, level = 1, sub.cluster = 5)
#####################################################################################

# To save files in csv:
write.csv(hb.results$partition.df, file = "hierbaps_partition.csv")
