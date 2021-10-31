# libraries --------------------------------------------------------------------
library(tidyverse)
library(ggraph)
library(tidygraph)

# loading STOWA data -----------------------------------------------------------
STOWA <- read_tsv("STOWA dataset/0014786-210914110416597.csv")

# filter unique species only ---------------------------------------------------
STOWA_specieslist <- STOWA %>%
  distinct(speciesKey, .keep_all = TRUE) %>%
  select(kingdom, phylum, class, order, family, genus, species, scientificName, speciesKey) %>%
  filter(!is.na(speciesKey)) %>%
  na.omit()

Arthropoda <- STOWA_specieslist %>%
  filter(phylum == "Arthropoda")
Malacostraca <- STOWA_specieslist %>%
  filter(class == "Malacostraca")
taxonomy_AA <- STOWA_specieslist %>%
  filter(species == "Asellus aquaticus")

# creating dedrogram -----------------------------------------------------------

library(metacoder)
library(taxa)

# dendrogram of Arthropods -----------------------------------------------------
Arthropoda <- parse_tax_data(Arthropoda, class_cols = 1:7) %>%
  filter_taxa(taxon_names != "") %>%
  heat_tree(node_label = taxon_names,
            node_color = n_obs,
            node_size = n_obs,
            layout = "da")

# dendrogram of malacostraca ---------------------------------------------------
# you can find Asselus Aquaticus in this dendrogram if you follow its taxonomy -
Malacostraca <- parse_tax_data(Malacostraca, class_cols = 1:7) %>%
  filter_taxa(taxon_names != "") %>%
  heat_tree(node_label = taxon_names,
            node_color = n_obs,
            node_size = n_obs,
            layout = "da")

# Asselus Aquaticus taxonomy ---------------------------------------------------
AsellusAquaticus_dendrogram <- parse_tax_data(taxonomy_AA, class_cols = 1:7) %>%
  filter_taxa(taxon_names != "") %>%
  heat_tree(node_label = taxon_names)

# Asselus Aquaticus data -------------------------------------------------------

AsellusAquaticus <- STOWA %>%
  filter(species == "Asellus aquaticus")

write.csv(AsellusAquaticus, "Asellus Aquaticus.csv")


# Asselus Aquaticus mapping ----------------------------------------------------
