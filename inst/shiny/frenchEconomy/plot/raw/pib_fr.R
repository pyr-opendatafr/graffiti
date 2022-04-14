library(tidyverse)
library(insee)

# get INSEE datasets list
dataset = get_dataset_list()

# get INSEE series key (idbank) list
idbank_list = get_idbank_list()

# select idbanks 
df_idbank_list_selected =
  idbank_list %>%
  filter(nomflow == "CNT-2014-PIB-EQB-RF") %>% # Gross domestic product balance
  filter(dim1 == "T") %>% #quarter
  filter(dim4 == "PIB") %>% #GDP
  filter(dim6 == "TAUX") %>% #rate
  filter(dim10 == "CVS-CJO") %>%  #SA-WDA, seasonally adjusted, working day adjusted
  add_insee_title()

# extract selected idbanks list
idbank = df_idbank_list_selected %>% pull(idbank)

# get data from idbank
data = get_insee_idbank("010565692")

#plot
gg_pib = 
ggplot(data, aes(x = DATE, y = OBS_VALUE)) +
  geom_col() +
  ggtitle("French GDP growth rate, quarter-on-quarter, sa-wda") +
  labs(subtitle = sprintf("Last updated : %s", data$TIME_PERIOD[1]))

export_minio_graph(gg_pib, perim = "FI", folder_name = "pib_fr")


