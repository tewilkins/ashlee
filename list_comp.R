library(tidyverse)

list1 = read.csv("l1.csv", sep = ",", header = F)  # Human
list2 = read.csv("l2.csv", sep = ",", header = F)  # Dunnart
ipscs = read.table("iPSCs_vs_Fibroblasts_DE.txt", sep = "\t", header = T)

# Ensures only one comparison for each gene
l1 = unique(list1$V1) 
l2 = unique(list2$V1) 

# Select upreg/downreg
upreg = ipscs %>%
  filter(avg_log2FC > 0)

downreg = ipscs %>%
  filter(avg_log2FC < 0)


# Create dataframe with l1 and empty observation data  
df1 = data.frame(l1)
df1$found = 0

for (i in 1:nrow(df1)){
  df1$found[i] <- ifelse(df1$l1[i] %in% l2, 1, 0)
}

l5 = df1 %>%
  filter(found == 1) %>%
  select(-found)

colnames(l5) = "gene"

l5$log2fc = ipscs$avg_log2FC[match(l5$gene, ipscs$gene)]

write.csv(l5, file = "matched genes.csv", row.names = FALSE)

###

opossum = read.table("opossumsc.txt", sep = "\t", header = T)

BiocManager::install("org.Hs.eg.db")
BiocManager::install("org.Mm.eg.db")
BiocManager::install("org.Md.eg.db")
install.packages("babelgene")

library(babelgene)
library(org.Hs.eg.db)
library(org.Mm.eg.db)
library(org.Md.eg.db)
library(AnnotationDbi)
library(tidyverse)

oposs_epi = opossum %>%
  filter(cluster == "EarlyEpi" | cluster == "Epiblast")

oposs_orth = orthologs(genes = oposs_epi$gene, species = "gray short-tailed opossum", human = FALSE)

oposs_genes = oposs_orth$human_symbol

write.csv(oposs_orth, file = "opossum_orthologs.csv", row.names = FALSE)

# Create dataframe with l1 and empty observation data  
upreg_df = data.frame(unique(upreg$gene))
colnames(upreg_df) = "gene"
upreg_df$found = 0

for (i in 1:nrow(upreg_df)){
  upreg_df$found[i] <- ifelse(upreg_df$gene[i] %in% oposs_genes, 1, 0)
}

downreg_df = data.frame(unique(downreg$gene))
colnames(downreg_df) = "gene"
downreg_df$found = 0

for (i in 1:nrow(downreg_df)){
  downreg_df$found[i] <- ifelse(downreg_df$gene[i] %in% oposs_genes, 1, 0)
}

upreg_found = upreg_df %>%
  filter(found == 1) %>%
  select(-found)

downreg_found = downreg_df %>%
  filter(found == 1) %>%
  select(-found)

upreg_found$log2fc = ipscs$avg_log2FC[match(upreg_found$gene, ipscs$gene)]
downreg_found$log2fc = ipscs$avg_log2FC[match(downreg_found$gene, ipscs$gene)]

write.csv(upreg_found, file = "opossum_upreg.csv", row.names = FALSE)
write.csv(downreg_found, file = "opossum_downreg.csv", row.names = FALSE)
