
housing_construction = function(lang = "en"){
  
  idbank_list = 
    get_idbank_list("CONSTRUCTION-LOGEMENTS") %>% 
    filter(INDICATEUR == "NBRE_LOG_COM") %>% 
    filter(LOGEMENT == "0") %>%  
    filter(REF_AREA %in% c('FM', 'R44', 'R75', 'R28','R76', 'R84', 
                           'R52', 'R93', 'R27', 'R53', 'R31', 'R11'))
  
  idbank_selected = idbank_list %>% pull(idbank)
  
  data = 
    get_insee_idbank(idbank_selected) %>% 
    add_insee_metadata() %>% 
    split_title()
  
  reg_order = 
    data %>% 
    filter(DATE == max(DATE)) %>% 
    arrange(desc(OBS_VALUE))
  
  reg_order_fr = reg_order %>% pull(TITLE_FR4)
  reg_order_en = reg_order %>% pull(TITLE_EN4)
  
  data_plot = data %>% 
    mutate(TITLE_FR4 = factor(TITLE_FR4, levels = reg_order_fr)) %>% 
    mutate(TITLE_EN4 = factor(TITLE_EN4, levels = reg_order_en))
  
   
  if(lang == "en"){
    
    gg =
      ggplot(data_plot, aes(x = DATE, y = OBS_VALUE)) +
      geom_line() +
      geom_point() +
      facet_wrap(~TITLE_EN4, scales = "free", labeller = label_wrap_gen(50)) +
      ggtitle("Housing construction - started dwellings - 12-month aggregate")
    
  }else{
    
    gg =
      ggplot(data_plot, aes(x = DATE, y = OBS_VALUE)) +
      geom_line() +
      geom_point() +
      facet_wrap(~TITLE_FR4, scales = "free", labeller = label_wrap_gen(50)) +
      ggtitle("Construction de logements - nombre de logements commen\u00E9s - cumul sur 12 mois")
  
  }
  
  gg = gg %>% add_style(lang = lang)
  
  gg = gg + theme(strip.text = element_text(size = 12))
  
  return(gg)
}

