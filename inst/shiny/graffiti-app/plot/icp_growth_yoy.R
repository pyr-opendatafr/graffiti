
icp_growth_yoy = function(lang = "en"){
  
  # idbank_list = get_idbank_list() %>%
  #   filter(str_detect(nomflow, "IPC-2015")) %>%
  #   filter(dim1 == "M") %>%
  #   filter(nchar(dim4) == 2) %>%
  #   filter(dim4 != "SO") %>% 
  #   add_insee_title() %>%
  #   filter(dim6 == "INDICE") %>%
  #   filter(dim7 == "ENSEMBLE") %>%
  #   filter(dim8 == "FE")
  # 
  # idbank_selected = idbank_list %>% pull(idbank)
  
  data =
    get_insee_idbank( "001763417", "001763491", "001763508", "001763529", "001763565", "001763620", "001763641", "001763683",
                       "001763698", "001763774", "001763781", "001763793", "001759970") %>%
    split_title() %>%
    mutate(month = lubridate::month(DATE)) %>% 
    arrange(DATE) %>% 
    group_by(TITLE_FR6, month) %>%
    mutate(growth = 100 * (OBS_VALUE / dplyr::lag(OBS_VALUE) - 1 )) %>% 
    filter(DATE >= "2010-01-01") %>% 
    mutate(TITLE_FR6 = case_when(is.na(TITLE_FR6) ~ TITLE_FR5, 
                                 TRUE ~as.character(TITLE_FR6))) %>% 
    mutate(TITLE_EN6 = case_when(is.na(TITLE_EN6) ~ TITLE_EN5, 
                                 TRUE ~as.character(TITLE_EN6))) 
  
  last_values = 
    data %>% group_by(TITLE_FR) %>% 
    filter(DATE == max(DATE)) %>% 
    mutate(growth_last = round(growth, 2)) %>% 
    select(IDBANK, growth_last)
  
  data = data %>% 
    left_join(last_values, by = "IDBANK") %>% 
    mutate(growth_last = case_when(growth_last > 0 ~ paste0("+", growth_last),
                                   TRUE ~ as.character(growth_last))) %>% 
    mutate(TITLE_FR6 = paste0(TITLE_FR6, " : ", growth_last, "%")) %>% 
    mutate(TITLE_EN6 = paste0(TITLE_EN6, " : ", growth_last, "%")) %>% 
    mutate(TITLE_EN6 = gsub("and routine household maintenance", "", TITLE_EN6)) %>% 
    mutate(TITLE_FR6 = gsub("et entretien courant du foyer|et autres combustibles", "", TITLE_FR6)) %>% 
    filter(!str_detect(TITLE_FR6, "Education|Enseignement"))
  
  col_order_en = unique(data$TITLE_EN6)
  col_order_fr = unique(data$TITLE_FR6)
  
  col_order_en = c(col_order_en[str_detect(col_order_en, "All items")],
                   col_order_en[!str_detect(col_order_en, "All items")])
  
  col_order_fr = c(col_order_fr[str_detect(col_order_fr, "Ensemble")],
                    col_order_fr[!str_detect(col_order_fr, "Ensemble")])
  
  data = data %>%
    mutate(TITLE_FR6 = factor(TITLE_FR6, levels = col_order_fr)) %>% 
    mutate(TITLE_EN6 = factor(TITLE_EN6, levels = col_order_en))
  
  gg =
    ggplot(data, aes(x = DATE, y = growth)) +
    geom_col(show.legend = FALSE) 
  
  if(lang == "en"){
    
    gg =
      gg +
      facet_wrap(~TITLE_EN6, scales = "free", labeller = label_wrap_gen(30)) +
      ggtitle("Inflation at coicop level 2, yearly growth rate")
    
  }else{
    
    gg =
      gg +
      facet_wrap(~TITLE_FR6, scales = "free", labeller = label_wrap_gen(30)) +
      ggtitle("Inflation au niveau coicop 2, glissement annuel")
      
    
  }
  
  # add_style function is available on GitHub
  gg = gg %>% add_style(lang = lang)
  gg = gg + ggplot2::theme(text = ggplot2::element_text(size = 12))
    
  return(gg)
}

