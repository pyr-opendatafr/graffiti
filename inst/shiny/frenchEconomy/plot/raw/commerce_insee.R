
library(insee)
library(tidyverse)
library(RColorBrewer)

A17 = c("AZ","DE","C1","C2","C3","C4","C5","FZ","GZ","HZ","IZ","JZ","KZ","LZ","MN","OQ","RU")

idbank_list =
  get_idbank_list() %>%
  filter(str_detect(nomflow, "^COM-EXT")) %>%
  add_insee_title(lang = "fr") %>%
  # filter(!str_detect(dim3, "SO")) %>%
  filter(dim4 == "VALEUR_ABSOLUE") %>%
  filter(dim2 == "SCF") %>%
  filter(dim8 == "CVS-CJO") %>%
  filter(str_detect(dim3, paste0(c(paste0("^", A17, "$"), "^A17"), collapse = "|")))

idbank_selected = idbank_list %>% pull(idbank)

data =
  get_insee_idbank(idbank_selected) %>%
  split_title(lang = "fr") %>%
  filter(DATE >= "2005-01-01")

mycolors = c(brewer.pal(8, "Set1"), brewer.pal(8, "Set2"), brewer.pal(8, "Set3"))

ggplot(data, aes(x = DATE, y = OBS_VALUE, fill = TITLE_FR4)) +
  geom_col() +
  scale_fill_manual(values = mycolors) +
  ggtitle("Balance commerciale mensuelle des biens en France") +
  labs(subtitle = sprintf("Dernière date : %s", max(data$DATE)))

# A17 : missing
# "AZ" "FZ" "GZ" "HZ" "IZ" "KZ" "LZ" "OQ"
#
# [1] "Agriculture, sylviculture et pêche"
# [2] "Construction"
# [3] "Commerce ; réparation d'automobiles et de motocycles"
# [4] "Transports et entreposage"
# [5] "Hébergement et restauration"
# [6] "Activités financières et d'assurance"
# [7] "Activités immobilières"
# [8] "Administration publique, enseignement, santé humaine et action sociale"




