
time1 = Sys.time()

library(rdbnomics)
library(rsdmx)
library(ggthemes)
library(grid)
library(lubridate)
library(eurostat)
library(fredr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(RColorBrewer)
# library(pRev)

date = gsub("-","",Sys.Date())

start_time = as.Date("2010-01-01")

# EAECBBSI3121.M > ECB series key =  BSI.M.U2.N.R.LRE.X.1.A1.3000.Z01.E
# Eurozone Money and Banking Credit Institutions, Reserve Maintenance, 
# Excess Reserves, Maturity Not Applicable, World (All Entities)

# EAILM024.W > ?
# Eurozone Money and Banking Eurosystem, Total Assets/Liabilities 
# Units: Millions of Euro,

# EAECBILM0007.W > ECB series key = ILM.W.U2.C.A052.U2.EUR
# Eurozone Money and Banking Eurosystem, Longer-Term Refinancing Operations Units: Millions of Euro

# EAECBILM0014.W > ECB series key = ILM.W.U2.C.A071.U2.EUR
# Eurozone Money and Banking Eurosystem, Securities Held for Monetary Policy Purposes Units: Millions of Euro

path_ECB_FM = "https://sdw-wsrest.ecb.europa.eu/service/data/ILM/"
myUrl <- paste0(path_ECB_FM,"W.U2.C..U2.EUR")
data <- readSDMX2(myUrl)
data <- as.data.frame(data)

path_ECB_FM = "https://sdw-wsrest.ecb.europa.eu/service/data/ILM/"
myUrl <- paste0(path_ECB_FM,"W.U2.C.A020000.U4.Z06")
dt <- readSDMX2(myUrl)
dt <- as.data.frame(dt)

path_ECB_FM = "https://sdw-wsrest.ecb.europa.eu/service/data/ILM/"
myUrl <- paste0(path_ECB_FM,"W.U2.C.A040000.U4.EUR")
dt2 <- readSDMX2(myUrl)
dt2 <- as.data.frame(dt2)

path_ECB_FM = "https://sdw-wsrest.ecb.europa.eu/service/data/ILM/"
myUrl <- paste0(path_ECB_FM,"W.U2.C.A030000.U2.Z06")
dt3 <- readSDMX2(myUrl)
dt3 <- as.data.frame(dt3)

path_ECB_FM = "https://sdw-wsrest.ecb.europa.eu/service/data/ILM/"
myUrl <- paste0(path_ECB_FM,"W.U2.C.A010000.Z5.Z0Z")
gold <- readSDMX2(myUrl)
gold <- as.data.frame(gold)

path_ECB_FM = "https://sdw-wsrest.ecb.europa.eu/service/data/ILM/"
myUrl <- paste0(path_ECB_FM,'W.U2.C.A110000.Z5.Z01')
dt4 <- readSDMX2(myUrl)
dt4 <- as.data.frame(dt4)


gold = gold %>%
  mutate(TITLE = 'Gold') %>%
  select(TITLE, BS_ITEM, obsTime, obsValue)

names_df = intersect(names(dt), names(dt2))
dt = dt[,names_df]
dt2 = dt2[,names_df]

dt = rbind(dt, dt2) %>%
  group_by(obsTime) %>%
  summarise(obsValue = sum(obsValue)) %>%
  mutate(BS_ITEM = "A020000")

dt = dt %>%
  mutate(TITLE = 'Claims on non-euro area residents') %>%
  select(TITLE, BS_ITEM, obsTime, obsValue)

dt3 = dt3 %>%
  mutate(TITLE = 'Claims on euro area residents') %>%
  select(TITLE, BS_ITEM, obsTime, obsValue)

dt4 = dt4 %>%
  mutate(TITLE = 'Other assets') %>%
  select(TITLE, BS_ITEM, obsTime, obsValue)

data = data %>%
  select(TITLE, BS_ITEM, obsTime, obsValue)

data2 = rbind(data, gold, dt, dt3, dt4)

data_ = data2 %>%
  dplyr::filter(stringr::str_detect(BS_ITEM,"A")) %>%
  dplyr::filter(!stringr::str_detect(BS_ITEM,"A070000")) %>%
  dplyr::filter(!stringr::str_detect(BS_ITEM,"A050000")) %>%
  mutate(date = as.Date(str_c(str_replace(obsTime,"W",""), "-1"),
                        format="%Y-%U-%u")) %>%
  mutate(year_ = lubridate::year(date)) %>%
  mutate(year_month = paste0(year(date),month(date))) %>%
  dplyr::filter(year_ >= 2008) %>%
  mutate(TITLE = gsub(" - Eurosystem| denominated in euro","",TITLE)) %>%
  drop_na(TITLE)

caption_order = data_ %>%
  dplyr::filter(date == max(data_$date)) %>%
  arrange(desc(obsValue)) %>%
  pull(TITLE)


data_ = as.data.frame(data_)
data_[,"TITLE"] = factor(data_[,"TITLE"], levels = caption_order)
data_[,"obsValue"] = data_[,"obsValue"] / 1000

mycolors = c(brewer.pal(8, "Set1"), brewer.pal(8, "Set2"), brewer.pal(8, "Set3"))

min_scale = data_ %>%
  group_by(date) %>%
  summarize(bs = sum(obsValue)) %>%
  pull(bs) %>%
  min()

max_scale = data_ %>%
  group_by(date) %>%
  summarize(bs = sum(obsValue)) %>%
  pull(bs) %>%
  max()
  
subtitle_month = gsub("\\\\.","", lubridate::month(max(data_$date), label = TRUE))
subtitle_day = gsub("\\\\.","", lubridate::day(max(data_$date)))
subtitle_year = lubridate::year(max(data_$date)) 
xaxis_breaks = seq.Date(from = min(data_$date), to = max(data_$date), by = "3 months")

time_now = with_tz(now(), "Europe/Paris")
size_tot = sprintf("taille totale : %s milliards d'euros", round(max_scale))
graph_subtitle = sprintf("Dernier point : %s %s %s, source : ECB \nFait le : %s\n %s",
                         subtitle_day, subtitle_month, subtitle_year,time_now, size_tot)

coeff = 500
yaxis_breaks = seq(from = 0,
                   to = ceiling(max(max_scale, na.rm = TRUE)/ coeff) * coeff, by = coeff)

graph_ecb_bs =  
ggplot() +
  geom_area(data = data_, aes(x = date, y = obsValue, fill = TITLE)) +
  ggtitle("Bilan de la Banque Centrale Européenne - Eurosystem") +
  scale_fill_manual(values = mycolors) +
  scale_y_continuous(sec.axis = dup_axis(), breaks = yaxis_breaks) +
  scale_x_date(breaks = xaxis_breaks, date_labels = "%b %y", expand = c(0.01, 0.01)) +
  labs(y = "en millard d'euros", subtitle = graph_subtitle) +  
  theme_stata() +
  theme(
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 18),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.text.y  = element_text(angle = 0, hjust = 1),
    axis.title.x = element_blank(),
    # axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) 


time2 = Sys.time()
run_time = as.numeric(difftime(time2, time1, units = "secs"))

export_minio_graph(graph_ecb_bs,
             folder_name = "ecb_balance_sheet",
             # create_code_html = T,
             run_time = run_time,
             perim = "FI", update = TRUE)

