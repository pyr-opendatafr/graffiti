
gov_debt = function(lang = "en"){
  
  # idbank_list = get_idbank_list() %>%
  #   filter(str_detect(nomflow, "DETTE")) %>%
  #   add_insee_title(lang = "fr") %>%
  #   filter(dim3 == "S13") %>%
  #   filter(dim1 == "T") %>%
  #   filter(is.na(title4)) %>%
  #   filter(dim4 %in% c("F2", "F331", "F332", "F41", "F42"))
  # 
  # idbank_selected = idbank_list %>% pull(idbank)
  
  data =
    get_insee_idbank("010596745", "010596747", "010596748", "010596750", "010596751") %>%
    split_title()
  
  dette_tot = get_insee_idbank("010596744")
  
  last_value = dette_tot %>% filter(DATE == max(DATE)) %>% pull(OBS_VALUE)
  
  mycolors = c(RColorBrewer::brewer.pal(8, "Set1"), 
               RColorBrewer::brewer.pal(8, "Set2"), 
               RColorBrewer::brewer.pal(8, "Set3"))
  
  if(lang == "en"){
    
    var_order = data %>%
      filter(DATE == max(DATE)) %>% 
      arrange(desc(OBS_VALUE)) %>% 
      pull(TITLE_EN2)
    
    data = data %>% 
      mutate(TITLE_EN2 = factor(TITLE_EN2, levels = var_order))
    
    gg =
      ggplot(data = data) +
      geom_area(data = data, aes(x = DATE, y = OBS_VALUE, fill = TITLE_EN2)) +
      geom_line(data = dette_tot, aes(x = DATE, y = OBS_VALUE), size = 0.5) +
      geom_point(data = dette_tot, aes(x = DATE, y = OBS_VALUE)) +
      scale_fill_manual(values = mycolors) +
      labs(subtitle = sprintf("Last value : %s billions euros", last_value)) +
      ggtitle("Debt of the general government, Maastricht definition")
    
  }else{
    
    var_order = data %>%
      filter(DATE == max(DATE)) %>% 
      arrange(desc(OBS_VALUE)) %>% 
      pull(TITLE_FR2)
    
    data = data %>% 
      mutate(TITLE_FR2 = factor(TITLE_FR2, levels = var_order))
    
    gg =
      ggplot(data = data) +
      geom_area(data = data, aes(x = DATE, y = OBS_VALUE, fill = TITLE_FR2)) +
      geom_line(data = dette_tot, aes(x = DATE, y = OBS_VALUE), size = 0.5) +
      geom_point(data = dette_tot, aes(x = DATE, y = OBS_VALUE)) +
      scale_fill_manual(values = mycolors) +
      labs(subtitle = sprintf("Derni\u00E8re valeur : %s milliards d'euros", last_value)) +
      ggtitle("Dette des administrations publiques, crit\u00E9res de Maastricht")
    
  }
  
  gg = gg %>% add_style(lang = lang)
  
  return(gg)
}

