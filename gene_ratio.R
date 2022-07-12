de_report = read.table("DE_proteins_report.csv", header = T, sep = ",")

library(ggplot2)
library(tidyverse)
library(stringr)

de_report$gene_ratio =  de_report$Significant / de_report$Annotated

de_gg = de_report %>%
  ggplot(aes(x = gene_ratio, 
             y = Term, 
             color = pvalues,
             size = Significant)) +
  geom_point(stat = "identity") +
  scale_color_continuous(low = "red", high = "yellow") +
  labs(color = "P-value", size = "Count", x = "Gene Ratio (Significant:Annotated)", y = "Gene Ontology Term") +
  theme_bw()

de_gg

de_bar_gg = de_report %>%
  ggplot(aes(x = gene_ratio, 
             y = Term, 
             color = pvalues,
             size = Significant)) +
  geom_point(stat = "identity") +
  scale_color_continuous(low = "red", high = "yellow") +
  labs(color = "P-value", size = "Count", x = "Gene Ratio (Significant:Annotated)", y = "Gene Ontology Term") +
  theme_bw()

## Another one

cluster7 = read.table("cluster7.csv", header = T, sep = ",")
cluster11 = read.table("cluster11.csv", header = T, sep = ",")
upreg_fb = read.table("upreg_fb.csv", header = T, sep = ",")
upreg_ipsc = read.table("upreg_ipsc.csv", header = T, sep = ",")

cluster7$gene_ratio = cluster7$Count / cluster7$GeneList
cluster11$gene_ratio = cluster11$Count / cluster11$GeneList
upreg_fb$gene_ratio = upreg_fb$Count / upreg_fb$GeneList
upreg_ipsc$gene_ratio = upreg_ipsc$Count / upreg_ipsc$GeneList

cluster7$DescWrap = str_wrap(cluster7$Description, width = 50)
cluster11$DescWrap = str_wrap(cluster11$Description, width = 50)
upreg_fb$DescWrap = str_wrap(upreg_fb$Description, width = 50)
upreg_ipsc$DescWrap = str_wrap(upreg_ipsc$Description, width = 50)

upreg_fb$Group = "fb"
upreg_ipsc$Group = "ipsc"

upreg_all = rbind(upreg_fb, upreg_ipsc)

c7_gg = cluster7 %>%
  ggplot(aes(x = gene_ratio, 
             y = DescWrap, 
             fill = Count)) +
  geom_bar(stat = "identity") +
  scale_color_continuous(low = "red", high = "yellow") +
  labs(color = "P-value", size = "Count", x = "Gene Ratio (Significant:Annotated)", y = "Gene Ontology Term") +
  theme_bw()

c7_gg

c11_gg = cluster11 %>%
  ggplot(aes(x = gene_ratio, 
             y = DescWrap, 
             fill = Count)) +
  geom_bar(stat = "identity") +
  scale_color_continuous(low = "red", high = "yellow") +
  labs(color = "P-value", size = "Count", x = "Gene Ratio (Significant:Annotated)", y = "Gene Ontology Term") +
  theme_bw()

c11_gg

ipsc_gg = upreg_ipsc %>%
  ggplot(aes(x = gene_ratio, 
             y = DescWrap, 
             fill = Count)) +
  geom_bar(stat = "identity") +
  scale_color_continuous(low = "red", high = "yellow") +
  labs(color = "P-value", size = "Count", x = "Gene Ratio (Significant:Annotated)", y = "Gene Ontology Term") +
  theme_bw()

ipsc_gg

fb_gg = upreg_fb %>%
  ggplot(aes(x = gene_ratio, 
             y = DescWrap, 
             fill = Count)) +
  geom_bar(stat = "identity") +
  scale_color_continuous(low = "red", high = "yellow") +
  labs(color = "P-value", size = "Count", x = "Gene Ratio (Significant:Annotated)", y = "Gene Ontology Term") +
  theme_bw()

fb_gg

install.packages("gridExtra")
library(gridExtra)

grid.arrange(ipsc_gg, fb_gg, ncol = 2)


########################################
# Writing plots
# wide plots
tiff(file = "c7_wide.tiff", width=8, height=4, units="in", res=100, compression="lzw")
c7_gg
dev.off()

tiff(file = "c11_wide.tiff", width=8, height=4, units="in", res=100, compression="lzw")
c11_gg
dev.off()

tiff(file = "fb_wide.tiff", width=8, height=4, units="in", res=100, compression="lzw")
fb_gg
dev.off()

tiff(file = "ipsc_wide.tiff", width=8, height=4, units="in", res=100, compression="lzw")
ipsc_gg
dev.off()

# tall plots
tiff(file = "c7_long.tiff", width=8, height=9, units="in", res=100, compression="lzw")
c7_gg
dev.off()

tiff(file = "c11_long.tiff", width=8, height=9, units="in", res=100, compression="lzw")
c11_gg
dev.off()

tiff(file = "fb_long.tiff", width=8, height=9, units="in", res=100, compression="lzw")
fb_gg
dev.off()

tiff(file = "ipsc_long.tiff", width=8, height=9, units="in", res=100, compression="lzw")
ipsc_gg
dev.off()

