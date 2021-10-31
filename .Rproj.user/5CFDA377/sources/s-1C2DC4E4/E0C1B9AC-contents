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

STOWA_specieslist_bacteria <- STOWA_specieslist %>%
  filter(kingdom == "Bacteria")

STOWA_specieslist_protozoa <- STOWA_specieslist %>%
  filter(kingdom == "Protozoa")

STOWA_specieslist_animalia <- STOWA_specieslist %>%
  filter(kingdom == "Animalia")

  Mollusca <- STOWA_specieslist_animalia %>%
   filter(phylum == "Mollusca")
  Arthropoda <- STOWA_specieslist_animalia %>%
   filter(phylum == "Arthropoda")
  Annelida <- STOWA_specieslist_animalia %>%
    filter(phylum == "Annelida")
  Bryozoa <- STOWA_specieslist_animalia %>%
    filter(phylum == "Bryozoa")
  Rotifera <- STOWA_specieslist_animalia %>%
    filter(phylum == "Rotifera")
  Portifera <- STOWA_specieslist_animalia %>%
    filter(phylum == "Portifera")
  Cnidaria <- STOWA_specieslist_animalia %>%
    filter(phylum == "Cnidaria")
  Chordata <- STOWA_specieslist_animalia %>%
    filter(phylum == "Chordata")

STOWA_specieslist_chromista <- STOWA_specieslist %>%
  filter(kingdom == "Chromista")

STOWA_specieslist_plantae <- STOWA_specieslist %>%
  filter(kingdom == "Plantae")

  Tracheophyta <- STOWA_specieslist_plantae %>%
    filter(phylum == "Tracheophyta")
  Chlorophyta <- STOWA_specieslist_plantae %>%
    filter(phylum == "Chlorophyta")
  Marchantiophyta <- STOWA_specieslist_plantae %>%
    filter(phylum == "Marchantiophyta")
  Bryophyta <- STOWA_specieslist_plantae %>%
    filter(phylum == "Bryophyta")


# creating dedrogram -----------------------------------------------------------

library(metacoder)
library(taxa)

Bacteria <- parse_tax_data(STOWA_specieslist_bacteria, class_cols = 1:7) %>%
  filter_taxa(taxon_names != "") %>%
  heat_tree(node_label = taxon_names,
            node_color = n_obs,
            node_size = n_obs,
            layout = "da")

Protozoa <- parse_tax_data(STOWA_specieslist_protozoa, class_cols = 1:7) %>%
  filter_taxa(taxon_names != "") %>%
  heat_tree(node_label = taxon_names,
            node_color = n_obs,
            node_size = n_obs,
            layout = "da")



