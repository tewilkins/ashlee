meta_proteomes = read.csv("meta-proteome.csv", sep = ",", header = T)

#install.packages("cluster")
#install.packages("UpSetR")

library(dplyr)# ensuring reproducibility for sampling
library(tidyverse)
library(cluster)
library(UpSetR)
library(ComplexHeatmap)

spnames = names(meta_proteomes)

all_proteins = meta_proteomes %>%
  pivot_longer(1:length(meta_proteomes), names_to = "Species", values_to = "Protein") %>%
  filter(Protein != "")



all_proteins$PROTEIN = toupper(all_proteins$Protein)
unique_proteins = unique(all_proteins$PROTEIN)
pa_df = data.frame(unique_proteins)
colnames(pa_df) = "Protein"





for (i in 1:length(spnames)){  # iterate over all species
  newcol <- paste0(spnames[i])  # defines a new column with species[i] name
  pa_df[newcol] <- 0  # populates new column with species[i] name full of 0s
  species.iter = all_proteins[all_proteins$Species == paste0(newcol), ]  # creates a subset of biota dataframe where species[i] data exists
  for (j in 1:nrow(pa_df)){
    pa_df[j,newcol] <- ifelse(pa_df$Protein[j] %in% species.iter$PROTEIN, 1, 0)  # sets row to 1 if that species was observed for that sample
  }
}


# upset plot :(
spnames

plot_names = c('Protein',
               'Bovine (OF)',
               'Feline (OEV)',
               'Porcine (OEV)',
               'Dasyurid (OF)',
               'Equine (OF)',
               'Canine (OEV)',
               'Ovine (OF)',
               'Leporine (OF)',
               'Human (OF)')

plot_spnames = c('Bos taurus',
               'Felis catus',
               'Sus domesticus',
               'Sminthopsis crassicaudata',
               'Equus ferus',
               'Canis familiaris',
               'Ovis aries',
               'Oryctolagus cuniculus',
               'Homo sapiens')


names(pa_df) = plot_names

m2 = make_comb_mat(pa_df)
set_size(m2)
UpSet(m2)




# RID HUMANS AT ONCE

all_proteins_NOHUMANS = all_proteins %>%
  filter(all_proteins$Species != "HUMAN.OF")
unique_proteins_nh = unique(all_proteins_NOHUMANS$PROTEIN)
pa_df_NOHUMAN = data.frame(unique_proteins_nh)
colnames(pa_df_NOHUMAN) = "Protein"

spnames_nh = unique(all_proteins_NOHUMANS$Species)

for (i in 1:length(spnames_nh)){  # iterate over all species
  newcol <- paste0(spnames_nh[i])  # defines a new column with species[i] name
  pa_df_NOHUMAN[newcol] <- 0  # populates new column with species[i] name full of 0s
  species.iter = all_proteins_NOHUMANS[all_proteins_NOHUMANS$Species == paste0(newcol), ]  # creates a subset of biota dataframe where species[i] data exists
  for (j in 1:nrow(pa_df_NOHUMAN)){
    pa_df_NOHUMAN[j,newcol] <- ifelse(pa_df_NOHUMAN$Protein[j] %in% species.iter$PROTEIN, 1, 0)  # sets row to 1 if that species was observed for that sample
  }
}

plot_names_nh = c('Protein',
               'Bovine (OF)',
               'Feline (OEV)',
               'Porcine (OEV)',
               'Dasyurid (OF)',
               'Equine (OF)',
               'Canine (OEV)',
               'Ovine (OF)',
               'Leporine (OF)')

names(pa_df_NOHUMAN) = plot_names_nh

m3 = make_comb_mat(pa_df_NOHUMAN)
set_size(m3)
UpSet(m3)

# hierarchical cluster

protein_dist <- dist(pa_df, method = 'euclidean')
Hierar_cl <- hclust(protein_dist, method = "average")
plot(Hierar_cl)
