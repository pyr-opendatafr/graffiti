
library(insee)
library(tidyverse)

idbank_list = get_idbank_list() %>%
  filter(str_detect(nomflow, "^ODD")) %>%
  add_insee_title(lang = "fr") %>%
  filter(str_detect(dim3, "EMPREINTE"))

idbank_selected = idbank_list %>% pull(idbank)

data = get_insee_idbank(idbank_selected)

gg_empreinte_carbone =
ggplot(data, aes(x = DATE, y = OBS_VALUE)) +
  geom_line() +
  geom_point() +
  facet_wrap(~TITLE_FR, scales = "free") +
  ggthemes::theme_stata() +
  ggtitle("Empreinte carbone et empreinte matière")

export_minio_graph(gg_empreinte_carbone, perim = "FR-DEV-DUR", folder_name = "empr_carb")

