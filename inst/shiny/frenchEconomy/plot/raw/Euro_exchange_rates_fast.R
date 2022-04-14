
library(ggthemes)
library(lubridate)
library(tidyverse)
library(rwebstat)

start_time = today() %m-% months(12)

runtime_start = Sys.time()

list_series_id = c("EXR.D.JPY.EUR.SP00.A", "EXR.D.GBP.EUR.SP00.A",
                   "EXR.D.USD.EUR.SP00.A", "EXR.D.CNY.EUR.SP00.A")

data =  rwebstat::w_data(dataset_name = "EXR",
                         startPeriod = year(start_time),
                         series_name = paste0(list_series_id, collapse = "+"))

df = data %>% 
  pivot_longer(-date, names_to = "variable", values_to = "value") %>% 
  mutate(label = case_when(str_detect(variable, "EXR.D.JPY.EUR.SP00.A") ~ "YEN / EURO",
                           str_detect(variable, "EXR.D.USD.EUR.SP00.A") ~ "DOLLAR / EURO",
                           str_detect(variable, "EXR.D.GBP.EUR.SP00.A") ~ "POUND / EURO",
                           str_detect(variable, "EXR.D.CNY.EUR.SP00.A") ~ "YUAN / EURO"
                           )) %>% drop_na()

last_values = df %>% 
  group_by(label) %>% 
  dplyr::filter(date == max(date)) %>% 
  mutate(value = round(value, 2)) 

time_now = with_tz(now(), "Europe/Paris")

subtt = sprintf("Dernier point : %s, Fait le : %s\nSource : Banque de France\n",
                max(last_values$date), gsub("CET","", time_now))

for(i in 1:nrow(last_values)){
  subtt = paste0(subtt, sprintf(" %s : %s,  ", last_values[i,"label"],  last_values[i,"value"]))
}

graph_eur_exr_fast = 
  ggplot(data = df, aes(x = date, y = value, colour = label)) +
  facet_wrap(~label, scales = "free") +
  geom_line(size = 1) +
  labs(subtitle = subtt) +
  # scale_y_continuous(position = "right") +
  ggtitle("Taux de change de l'euro") +
  ggthemes::theme_stata() +
  theme(
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 18),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.text.y  = element_text(angle = 0, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    text = element_text(size = 14),
    strip.text.x = element_text(size = 14),
    legend.position = "bottom"
  ) 
  
runtime_end = Sys.time()
run_time = as.numeric(difftime(runtime_end, runtime_start), units = "secs")


export_minio_graph(
  graph_eur_exr_fast,
  # create_code_html =  TRUE,
  folder_name = "eur_exr_fast",
  perim = "FI",
  update = TRUE,
  run_time = run_time
)
  