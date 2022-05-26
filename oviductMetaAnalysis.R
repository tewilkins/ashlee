# Read dataset into R:
meta_proteomes = read.csv("meta-proteome.csv", sep = ",", header = T)

# These are 
#install.packages("cluster")
#install.packages("UpSetR")

# The libraries you need:
library(dplyr) # actually is included in tidyverse
library(tidyverse)
# library(cluster)
library(UpSetR)
# library(ComplexHeatmap)

# Find names of all species in the proteome dataset:
spnames = names(meta_proteomes)

# Put protein dataframe into long format and remove any blank cells (different to NA!)
all_proteins = meta_proteomes %>%
  pivot_longer(1:length(meta_proteomes), names_to = "Species", values_to = "Protein") %>%
  filter(Protein != "")

# Convert all proteins to uppercase:
all_proteins$PROTEIN = toupper(all_proteins$Protein)

# Remove duplicates:
unique_proteins = unique(all_proteins$PROTEIN)

# Convert to a dataframe for logical imputation:
pa_df = data.frame(unique_proteins)

# Set the name of the protein column to 'Protein':
colnames(pa_df) = "Protein"

# This loop creates a matrix of logical data for each protein under each species.
# A value of 1 = protein is present in that species
# A value of 0 = protein is absent in that species
for (i in 1:length(spnames)){   # iterates over all species
  newcol <- paste0(spnames[i])  # defines a new column with species[i] name
  pa_df[newcol] <- 0            # populates new column with species[i] name full of 0s
  species.iter = all_proteins[all_proteins$Species == paste0(newcol), ]  # creates a subset of biota dataframe where species[i] data exists
  for (j in 1:nrow(pa_df)){     # iterates over all proteins within each species
    pa_df[j,newcol] <- ifelse(pa_df$Protein[j] %in% species.iter$PROTEIN, 1, 0)  # sets row to 1 if that species was observed for that sample
  }
}

# upset plot :(

# Create a list of names that are better suited to the plot axis:
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

# Binomial nomenclature:
plot_spnames = c('Bos taurus',
               'Felis catus',
               'Sus domesticus',
               'Sminthopsis crassicaudata',
               'Equus ferus',
               'Canis familiaris',
               'Ovis aries',
               'Oryctolagus cuniculus',
               'Homo sapiens')

# Set column names in presence-absence dataframe:
names(pa_df) = plot_names

# Create a combination matrix of logical dataframe:
m2 = make_comb_mat(pa_df)

# Check the set size (i.e., number of proteins) for each species:
set_size(m2)

# Plot the UpSet Plot:
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
