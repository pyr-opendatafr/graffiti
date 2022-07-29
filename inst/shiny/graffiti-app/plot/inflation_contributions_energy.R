
inflation_contributions_energy = function(lang = 'en'){
 
  ids = get_idbank_list2("IPC-2015")
  
  indices = ids %>% 
    filter(COICOP2016 == "SO") %>% 
    filter(NATURE == "INDICE") %>% 
    filter(FREQ == "M") %>% 
    filter(MENAGES_IPC == "ENSEMBLE") %>% 
    filter(REF_AREA == "FE") %>% 
    filter(PRIX_CONSO %in% c("4000", "4034", "4003", "4009")) %>% 
    pull(idbank)
  
  id_nrj = ids %>% 
    filter(COICOP2016 %in% c("04521","04522", "0451", "0453",
                             "0454", "07221", "07222", "07223")) %>% 
    filter(NATURE == "POND") %>% 
    filter(MENAGES_IPC == "ENSEMBLE") %>% 
    filter(REF_AREA == "FE") %>% 
    filter(PRIX_CONSO %in% c("SO")) %>% 
    select(idbank, COICOP2016, COICOP2016_label_fr, NATURE)
  
  id_nrj = ids %>% 
    filter(COICOP2016 %in% c("04521","04522", "0451", "0453", "0454", "07221", "07222", "07223")) %>% 
    filter(NATURE == "INDICE") %>% 
    filter(FREQ == "M") %>% 
    filter(MENAGES_IPC == "ENSEMBLE") %>% 
    filter(REF_AREA == "FE") %>% 
    filter(PRIX_CONSO %in% c("SO")) %>% 
    select(idbank, COICOP2016, COICOP2016_label_fr, NATURE)
  
  id_nrj = ids %>% 
    filter(COICOP2016 %in% c("0452", "0451", "0453", "0454", "07221", "07222", "07223")) %>% 
    filter(NATURE == "INDICE") %>% 
    filter(FREQ == "M") %>% 
    filter(MENAGES_IPC == "ENSEMBLE") %>% 
    filter(REF_AREA == "FE") %>% 
    filter(PRIX_CONSO %in% c("SO")) %>% 
    select(idbank, COICOP2016, COICOP2016_label_fr, NATURE)
  
  nrj = id_nrj %>% pull(idbank)
  
  indices = c(indices, nrj)
  
  pond_id = ids %>% 
    filter(NATURE == "POND") %>% 
    filter(COICOP2016 == "SO") %>% 
    filter(PRIX_CONSO %in% c("4000", "4034", "4003", "4009")) %>% 
    filter(MENAGES_IPC == "ENSEMBLE") %>% 
    filter(REF_AREA == "FE") %>% 
    pull(idbank)
  
  pond_id_nrj = ids %>% 
    filter(NATURE == "POND") %>% 
    filter(COICOP2016 %in% c("0452", "0451", "0453", "0454", "07221", "07222", "07223")) %>% 
    filter(PRIX_CONSO %in% c("SO")) %>% 
    filter(MENAGES_IPC == "ENSEMBLE") %>% 
    filter(REF_AREA == "FE") %>% 
    pull(idbank)
  
  pond_id = c(pond_id, pond_id_nrj)
  
  ponderation = 
    get_insee_idbank(pond_id) %>% 
    add_insee_metadata2() %>% 
    group_by(DATE) %>% 
    mutate(ponderation = OBS_VALUE/sum(OBS_VALUE)) %>% 
    ungroup() %>% 
    mutate(year = year(DATE)) %>% 
    select(year, ponderation, COICOP2016, PRIX_CONSO) 
  
  indicesTot = ids %>% 
    filter(NATURE == "INDICE") %>% 
    filter(FREQ == "M") %>% 
    filter(MENAGES_IPC == "ENSEMBLE") %>% 
    filter(COICOP2016 == "00") %>% 
    filter(REF_AREA == "FE") %>% 
    filter(PRIX_CONSO == "SO") %>% 
    pull(idbank) %>% 
    get_insee_idbank() %>% 
    arrange(DATE) %>% 
    mutate(growth = 100 * (OBS_VALUE/dplyr::lag(OBS_VALUE, 12)-1)) %>% 
    select(DATE, inflationTot = OBS_VALUE, growth) %>% 
    filter(DATE >= "2006-01-01")
  
  inflationTotDec = indicesTot %>% 
    mutate(month = month(DATE), year = year(DATE)) %>% 
    filter(month == 12) %>% 
    select(year, inflationTotDec = inflationTot)
  
  df = get_insee_idbank(indices) %>% 
    add_insee_metadata2() %>% 
    arrange(DATE) %>% 
    group_by(COICOP2016, PRIX_CONSO) %>% 
    mutate(growth = 100 * (OBS_VALUE/dplyr::lag(OBS_VALUE, 12)-1)) %>% 
    ungroup() %>% 
    filter(DATE >= "2006-01-01") %>% 
    mutate(year = year(DATE)) %>% 
    mutate(month = month(DATE)) 
  
  dfDec = df %>% 
    ungroup() %>% 
    mutate(month = month(DATE)) %>% 
    filter(month == 12) %>% 
    select(year, COICOP2016, OBS_VALUE_DEC = OBS_VALUE, PRIX_CONSO)
  
  data = df %>% 
    left_join(dfDec, by = c("year", "COICOP2016", "PRIX_CONSO")) %>% 
    left_join(ponderation, by = c("year", "COICOP2016", "PRIX_CONSO")) %>% 
    left_join(indicesTot, by = "DATE") %>% 
    left_join(inflationTotDec, by = "year") %>% 
    select(DATE, year, OBS_VALUE, OBS_VALUE_DEC, COICOP2016,
           COICOP2016_label_en, COICOP2016_label_fr,
           month, PRIX_CONSO, PRIX_CONSO_label_fr, PRIX_CONSO_label_en,
           ponderation, inflationTot, inflationTotDec) %>% 
    group_by(month, COICOP2016, PRIX_CONSO) %>% 
    mutate(contrib1 = ponderation * lag(inflationTotDec) * (OBS_VALUE/lag(OBS_VALUE_DEC) - 1)) %>% 
    mutate(contrib2 = lag(ponderation) *
             lag(inflationTotDec,2)/lag(OBS_VALUE_DEC,2) *
             (lag(OBS_VALUE_DEC,1) - lag(OBS_VALUE))) %>% 
    mutate(contrib = 100*(contrib1 + contrib2)/lag(inflationTot)) %>% 
    filter(DATE >= "2008-01-01") %>% 
    mutate(PRIX_CONSO_label_fr = case_when(PRIX_CONSO == "SO" ~ COICOP2016_label_fr,
                                           TRUE ~ as.character(PRIX_CONSO_label_fr))) %>% 
    mutate(PRIX_CONSO_label_en = case_when(PRIX_CONSO == "SO" ~ COICOP2016_label_en,
                                           TRUE ~ as.character(PRIX_CONSO_label_en)))
  
  indicesTot2 = indicesTot %>% 
    filter(DATE >= "2008-01-01")
  
  checks = data %>% 
    group_by(DATE) %>% 
    summarise(contribTot = sum(contrib)) %>% 
    filter(DATE >= "2008-01-01") %>% 
    left_join(indicesTot2, by = "DATE") %>% 
    mutate(diff = contribTot - growth) 
  
  
  next_year = year(today()) + 1 
  axisx = seq.Date(from = as.Date("1990-01-01"), to = as.Date(paste0(next_year, "-01-01")), by = "1 year")
  
  prix_conso_level_fr = c("Alimentation", 
                       "04.5.1 - Électricité",
                       "04.5.2 - Gaz",
                       "07.2.2.1 - Gazole",
                       "07.2.2.2 - Essence",
                       "04.5.3 - Combustibles liquides",
                       "04.5.4 - Combustibles solides",
                       "07.2.2.3 - Autres carburants pour véhicules personnels",
                       "Produits manufacturés",
                       "Services",
                       "Tabac")
  
  prix_conso_level_en = c("Food",
                          "04.5.1 - Electricity",
                          "04.5.2 - Gas",
                          "07.2.2.1 - Diesel",
                          "07.2.2.2 - Petrol",
                          "04.5.3 - Liquid fuels",
                          "04.5.4 - Solid fuels",
                          "07.2.2.3 - Other fuels for personal transport equipment",
                          "Manufactured products",
                          "Services",
                          "Tobacco")
  
  data2 = data %>% 
    mutate(PRIX_CONSO_label_fr = factor(PRIX_CONSO_label_fr, levels = prix_conso_level_fr)) %>% 
    mutate(PRIX_CONSO_label_en = factor(PRIX_CONSO_label_en, levels = prix_conso_level_en))
  
  maxdate = data2 %>% filter(COICOP2016 == '07222') %>% pull(DATE) %>% max()
  data2 = data2 %>% filter(DATE <= maxdate)
  indicesTot2 = indicesTot2 %>% filter(DATE <= maxdate)
  checks = checks %>% filter(DATE <= maxdate)
  
  colors2 = c(RColorBrewer::brewer.pal(9, "Set1")[1],
              brewer.pal(9,"Blues")[9:3],
              RColorBrewer::brewer.pal(9, "Set1")[c(-1,-2,-6)])
  
  if (lang == 'fr'){
    fillcol = "PRIX_CONSO_label_fr"
    title = "Inflation, contributions et energie"
  }
  if (lang == 'en'){
    fillcol = "PRIX_CONSO_label_en"
    title = "Inflation, contributions and energy"
  }
  
  gg =
    ggplot() +
    geom_col(data = data2, aes_string(x = "DATE", y = "contrib", fill = fillcol)) +
    geom_line(data = indicesTot2, aes(x = DATE, y = growth)) +
    geom_point(data = indicesTot2, aes(x = DATE, y = growth)) +
    geom_point(data = checks, aes(x = DATE, y = contribTot), shape= 3) +
    scale_fill_manual(values = colors2) +
    scale_y_continuous(sec.axis = dup_axis(labels = function(x) paste0(x, "%")),
                       labels = function(x) paste0(x, "%")) +
    scale_x_date(expand = c(0, 0.1), breaks = axisx, date_labels = "%Y") +
    ggtitle(title) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_blank()) + 
    guides(fill = guide_legend(ncol = 4))
  
  gg = gg %>% add_style(lang = lang)
  
  return(gg)
}