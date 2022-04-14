library(insee)
library(tidyverse)

idbank_list = get_idbank_list() %>%
  filter(str_detect(nomflow, "IPC-2015")) %>%
  filter(dim1 == "M") %>%
  filter(nchar(dim4) == 3) %>%
  add_insee_title() %>%
  filter(dim6 == "INDICE") %>%
  filter(dim7 == "ENSEMBLE") %>%
  filter(dim8 == "FE")

idbank_selected = idbank_list %>% pull(idbank)

data =
  get_insee_idbank(idbank_selected) %>%
  split_title(lang = "fr") %>%
  filter(DATE >= "2010-01-01") %>%
  group_by(TITLE_FR6) %>%
  mutate(growth = 100 * (OBS_VALUE / dplyr::lag(OBS_VALUE) - 1 )) %>%
  mutate(TITLE_FR6 = substr(TITLE_FR6, 1, 30))

ggplot(data, aes(x = DATE, y = OBS_VALUE)) +
  geom_line() +
  facet_wrap(~TITLE_FR6, scales = "free")

ggplot(data, aes(x = DATE, y = growth)) +
  geom_line() +
  facet_wrap(~TITLE_FR6, scales = "free") +
  ggtitle("Inflation au niveau coicop 3, glissement mensuel")

