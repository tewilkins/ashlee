de_report = read.table("DE_proteins_report.tsv", header = T, sep = "\t")

library(ggplot2)
library(tidyverse)

de_report$gene_ratio =  de_report$Significant / de_report$Annotated

de_gg = de_report %>%
  ggplot(aes(x = gene_ratio, 
             y = Term, 
             fill = Significant)) +
  geom_bar(stat = "identity")

de_gg
