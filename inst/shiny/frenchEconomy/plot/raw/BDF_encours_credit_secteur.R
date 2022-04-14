
library(tidyverse)
library(lubridate)
library(rwebstat)

runtime_start = Sys.time()

# catalogue = w_datasets()

df = rwebstat::w_data(dataset_name = "DIREN", series_name = "M.FR.CR.LME.EN.01.N.*.TT")
# df2 = rwebstat::w_data(dataset_name = "DIREN", series_name = "M.FR.CR.LME.EN+ME.01.N.*.TT")
# 07 a la place de 01 pour obtenir les glissements plutot que les encours

metadata = rwebstat::w_meta(df) %>% 
  slice(2, 4) %>% 
  select(-1) %>% 
  t() %>% 
  as.data.frame()

metadata[,"variable"] = as.character(rownames(metadata))
names(metadata) = c("title", "title_long", "variable")

data = df %>%
  pivot_longer(-date, names_to = "variable", values_to = "value") %>% 
  left_join(metadata) %>% 
  separate(title, paste0("col", 1:3), sep = "\\,", remove = F) %>% 
  mutate(title_ = gsub("Crédits mobilisés et mobilisables", "", title)) %>% 
  filter(variable != "DIREN.M.FR.CR.LME.EN.01.N.NC.TT") %>% 
  mutate(col2 = case_when(variable == "DIREN.M.FR.CR.LME.EN.01.N.ZZ.TT" ~ "TOTAL", 
                          TRUE ~ as.character(col2)))
# exclusion des non classés

# test = data %>% filter(is.na(col3))

last_values_date = 
  data %>% 
  pull(date) %>%
  max()

time_now = with_tz(now(), "Europe/Paris")

gg_credit_secteur = 
ggplot(data, aes(x = date, y = value, colour = col2)) +
  geom_line() +
  geom_point(size = 1) +
  facet_wrap(~col2, scales = "free") +
  ggtitle("Crédits mobilisés et mobilisables des entreprises") + 
  labs(subtitle = sprintf("Dernier point : %s, Fait le : %s", last_values_date, time_now)) +
  ggthemes::theme_stata() +
  theme(
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 18),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.text.y  = element_text(angle = 0, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) 

runtime_end = Sys.time()
run_time = as.numeric(difftime(runtime_end, runtime_start), units = "secs")

export_minio_graph(gg_credit_secteur,
                   perim = "FI", folder_name = "credit_fr_secteur",
                   update = TRUE, run_time= run_time)
