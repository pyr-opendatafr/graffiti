library(insee)
library(tidyverse)

dt = get_dataset_list()

idbank_list = 
  get_idbank_list("CNA-2014-ERE") %>% 
  filter(PRIX_REF == "VAL") %>% 
  filter(CNA_PRODUIT == "NNTOTAL")

idbank_selected = idbank_list %>% pull(idbank)

data = 
  get_insee_idbank(idbank_selected) %>% 
  add_insee_metadata() %>% 
  split_title()

ggplot(data, aes(x = DATE, y = OBS_VALUE)) +
  geom_line() +
  facet_wrap(~TITLE_FR, scales = "free")