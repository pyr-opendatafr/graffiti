
inflation_contributions = function(lang = 'en'){
 
  colors = c(RColorBrewer::brewer.pal(9, "Set1")[-6], RColorBrewer::brewer.pal(8, "Set2")[c(-2,-4)])
  
  # LISTE DES SERIES DU DATASET INFLATION
  ids = get_idbank_list2("IPC-2015")
  
  indices = ids %>% 
    filter(COICOP2016 == "SO") %>% 
    filter(NATURE == "INDICE") %>% 
    filter(FREQ == "M") %>% 
    filter(MENAGES_IPC == "ENSEMBLE") %>% 
    filter(REF_AREA == "FE") %>% 
    filter(PRIX_CONSO %in% c("4000", "4034", "4003", "4007", "4009")) %>% 
    pull(idbank)
  
  pond_id = ids %>% 
    filter(NATURE == "POND") %>% 
    filter(COICOP2016 == "SO") %>% 
    filter(PRIX_CONSO %in% c("4000", "4034", "4003", "4007", "4009")) %>% 
    filter(MENAGES_IPC == "ENSEMBLE") %>% 
    filter(REF_AREA == "FE") %>% 
    pull(idbank)
  
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
           COICOP2016_label_fr, month, PRIX_CONSO, PRIX_CONSO_label_fr,
           PRIX_CONSO_label_en,
           ponderation, inflationTot, inflationTotDec) %>% 
    group_by(month, COICOP2016, PRIX_CONSO) %>% 
    mutate(contrib1 = ponderation * lag(inflationTotDec) * (OBS_VALUE/lag(OBS_VALUE_DEC) - 1)) %>% 
    mutate(contrib2 = lag(ponderation) *
             lag(inflationTotDec,2)/lag(OBS_VALUE_DEC,2) *
             (lag(OBS_VALUE_DEC,1) - lag(OBS_VALUE))) %>% 
    mutate(contrib = 100*(contrib1 + contrib2)/lag(inflationTot)) %>% 
    filter(DATE >= "2008-01-01")
  
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
  
  if (lang == "fr"){
    title = "Inflation et contributions"
    fill_column = "PRIX_CONSO_label_fr"
  }
  if (lang == "en"){
    title = "Inflation and contributions"
    fill_column = "PRIX_CONSO_label_en"
    data = data %>% 
      mutate(PRIX_CONSO_label_en = factor(PRIX_CONSO_label_en,
                                          levels = c('Food', 'Energy',
                                                     'Manufactured products',
                                                     'Services', 'Tobacco')))
  }
  
  gg = 
    ggplot() +
      geom_col(data = data, aes_string(x = 'DATE', y = 'contrib',
                                       fill = fill_column)) +
      geom_line(data = indicesTot2, aes(x = DATE, y = growth)) +
      geom_point(data = indicesTot2, aes(x = DATE, y = growth)) +
      geom_point(data = checks, aes(x = DATE, y = contribTot), shape= 3) +
      scale_fill_manual(values = colors) +
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