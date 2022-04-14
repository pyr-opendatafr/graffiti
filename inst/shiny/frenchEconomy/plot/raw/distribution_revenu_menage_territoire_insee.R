library(insee)
library(tidyverse)

dataset = get_dataset_list()

idbank_list =
  get_idbank_list() %>%
  filter(str_detect(nomflow, "TCRED-SALAIRES-REVENUS-MEN")) %>%
  add_insee_title(lang = "fr")

# exemple avec la population
# https://hadrilec.github.io/insee/articles/v6_pop_map-vignettes.html
