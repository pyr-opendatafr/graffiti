

dataset_list = get_dataset_list()

idbank_list = get_idbank_list() %>%
  filter(nomflow == "ENQ-CONJ-INV-IND") %>%
  add_insee_title(lang = "fr") %>%
  filter(dim3 == "CZ") %>%
  filter(dim9 == "AVRS") %>%
  filter(dim2 == "ECII_MOTIVATIONS_ECONOMIQUES") %>%
  filter(dim6 == "Y1H")

list_idbank_selected = idbank_list %>% pull(idbank)

data = get_insee_idbank(list_idbank_selected)


gg_pierre =
ggplot(data, aes(x = DATE, y = OBS_VALUE)) +
  geom_line() +
  ggtitle("Part des investissements consacrée au renouvellement des équipements en place")

export_minio_graph(gg_pierre, perim = "FR", folder_name = "inv_renouv")
