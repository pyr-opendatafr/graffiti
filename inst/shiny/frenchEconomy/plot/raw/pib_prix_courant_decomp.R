library(RColorBrewer)
library(tidyverse)
library(lubridate)
library(insee)

#
idbank_list = get_idbank_list() %>%
  filter(nomflow == "CNT-2014-PIB-EQB-RF") %>%
  filter(dim1 == "T") %>%
  add_insee_title(lang = "fr") %>%
  filter(dim10 == "CVS-CJO") %>%
  filter(dim6 == "VALEUR_ABSOLUE") %>%
  filter(dim8 == "V") %>%
  filter(!str_detect(dim4, "P51[:alpha:]|P52|P53|^P4|^SOLDE|^DIN|PIB|^D|^P3[:digit:]"))

idbank_selected = idbank_list %>% pull(idbank)

data = get_insee_idbank("010565709", "010565711", "010565731", "010565736",
                        "010565723", "010565724", "010565726") %>%
  left_join(idbank_list, by = c("IDBANK" = "idbank")) %>%
  mutate(TITLE_FR = paste(dim4, "-", TITLE_FR)) %>%
  mutate(OBS_VALUE = case_when(IDBANK == "010565726" ~ - OBS_VALUE/1000, TRUE ~ as.numeric(OBS_VALUE)/1000))

pib = get_insee_idbank("010565707") %>%
  mutate(OBS_VALUE = OBS_VALUE / 1000)

pib_short = pib %>%
  mutate(pib = OBS_VALUE) %>%
  select(DATE, pib)

contrib = data %>%
  group_by(DATE) %>%
  summarise(contrib = sum(OBS_VALUE)) %>%
  drop_na() %>%
  select(DATE, contrib) %>%
  left_join(pib_short) %>%
  mutate(check = pib - contrib)

start_date = as.Date("2008-01-01")

pib_plot = pib %>%
  filter(DATE >= start_date)

data_plot = data %>%
  filter(DATE >= start_date)

colors_ = c(brewer.pal(11, 'Spectral')[2], brewer.pal(9, 'Pastel1')[1],brewer.pal(7,'Set1')[1],
            brewer.pal(7,'Set1')[-1], brewer.pal(8,'Set2'), brewer.pal(11,'Set3'),
            brewer.pal(9, 'Pastel1'), brewer.pal(8, 'Pastel2'),
            brewer.pal(8, 'Dark2'), brewer.pal(12, 'Paired'))

gg_pib_courant =
  ggplot() +
  geom_col(data = data_plot, aes(x = DATE, y = OBS_VALUE, fill = TITLE_FR)) +
  geom_point(data = pib_plot,  aes(x = DATE, y = OBS_VALUE, colour = TITLE_FR)) +
  geom_line(data = pib_plot,  aes(x = DATE, y = OBS_VALUE, colour = TITLE_FR)) +
  ggthemes::theme_stata() +
  guides(fill = guide_legend(ncol = 1)) +
  scale_x_date(expand = c(0.01, 0.01)) +
  scale_colour_manual(values = "black") +
  scale_fill_manual(values = colors_) +
  ggtitle("PIB courant - décomposition, milliards d'euros") +
  labs(subtitle = sprintf("Dernier point : %s", max(data_plot$DATE))) +
  theme(
    plot.subtitle = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0.5),
    axis.text.y = element_text(angle = 0),
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

gg_pib_courant