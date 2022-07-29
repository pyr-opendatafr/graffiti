
pib_current_price = function(lang = "en"){
  
  # idbank_list = 
  #   get_idbank_list("CNT-2014-PIB-EQB-RF") %>%
  #   filter(FREQ == "T") %>%
  #   filter(CORRECTION == "CVS-CJO") %>%
  #   filter(NATURE == "VALEUR_ABSOLUE") %>%
  #   filter(VALORISATION == "V") %>%
  #   filter(!str_detect(OPERATION, "P51[:alpha:]|P52|P53|^P4|^SOLDE|^DIN|PIB|^D|^P3[:digit:]"))

  # idbank_selected = idbank_list %>% pull(idbank)
  
  data = get_insee_idbank("010565709", "010565711", "010565731", "010565736",
                          "010565723", "010565724", "010565726") %>%
    add_insee_metadata2() %>% 
    split_title() %>% 
    mutate(TITLE_FR = paste(OPERATION, "-", TITLE_FR1)) %>%
    mutate(TITLE_EN = paste(OPERATION, "-", TITLE_EN1)) %>%
    mutate(OBS_VALUE = case_when(IDBANK == "010565726" ~ - OBS_VALUE/1000,
                                 TRUE ~ as.numeric(OBS_VALUE)/1000))
  
  pib = get_insee_idbank("010565707") %>%
    mutate(OBS_VALUE = OBS_VALUE / 1000)
  
  pib_short = pib %>%
    mutate(pib = OBS_VALUE) %>%
    select(DATE, pib)
  
  contrib = data %>%
    group_by(DATE) %>%
    summarise(contrib = sum(OBS_VALUE)) %>%
    tidyr::drop_na() %>%
    select(DATE, contrib) %>%
    left_join(pib_short) %>%
    mutate(check = pib - contrib)
  
  start_date = as.Date("2008-01-01")
  
  pib_plot = pib %>%
    filter(DATE >= start_date)
  
  data_plot = data %>%
    filter(DATE >= start_date)
  
  colors_ = c(RColorBrewer::brewer.pal(11, 'Spectral')[2],
              RColorBrewer::brewer.pal(9, 'Pastel1')[1],
              RColorBrewer::brewer.pal(7,'Set1')[1],
              RColorBrewer::brewer.pal(7,'Set1')[-1],
              RColorBrewer::brewer.pal(8,'Set2'))
  
  if(lang == "en"){
    
    gg =
      ggplot(data = data_plot) +
      geom_col(data = data_plot, aes(x = DATE, y = OBS_VALUE, fill = TITLE_EN)) +
      geom_point(data = pib_plot,  aes(x = DATE, y = OBS_VALUE, colour = TITLE_EN)) +
      geom_line(data = pib_plot,  aes(x = DATE, y = OBS_VALUE, colour = TITLE_EN)) +
      guides(fill = guide_legend(ncol = 2)) +
      scale_colour_manual(values = "black") +
      scale_fill_manual(values = colors_) +
      ggtitle("GDP current prices, billions euros")
    
  }else{
    
    gg =
      ggplot(data = data_plot) +
      geom_col(data = data_plot, aes(x = DATE, y = OBS_VALUE, fill = TITLE_FR)) +
      geom_point(data = pib_plot,  aes(x = DATE, y = OBS_VALUE, colour = TITLE_FR)) +
      geom_line(data = pib_plot,  aes(x = DATE, y = OBS_VALUE, colour = TITLE_FR)) +
      guides(fill = guide_legend(ncol = 2)) +
      scale_colour_manual(values = "black") +
      scale_fill_manual(values = colors_) +
      ggtitle("PIB courant - d\u00E9composition, milliards d'euros")
      
    
  }
  
  gg = gg %>% add_style(lang = lang)
    
  return(gg)
}

