
ippmp = function(lang = "en"){
  
  # idbank_list =
  #   get_idbank_list("IPPMP-NF") %>%
  #   filter(NATURE == "INDICE") %>%
  #   filter(REF_AREA == "FM") %>%
  #   filter(MONNAIE == "E") %>%
  #   add_insee_title()
  # 
  # idbank_selected = idbank_list %>% pull(idbank)
  
  data =
    get_insee_idbank("010002010", "010001994", "010002008", "010002024", "010002002",
                     "010002028", "010002014", "010002012", "010002000", "010001998", "010600341",
                     "010002006", "010002004", "010001992", "010002018", "010002016", "010002022",
                     "010002020", "010001996", "010002038", "010002040", "010002103") %>%
    split_title()
  
  
  if(lang == "en"){
    
    gg =
      ggplot(data, aes(x = DATE, y = OBS_VALUE)) +
      facet_wrap(~TITLE_EN2, scales = "free") +
      geom_line() +
      labs(subtitle = "Prices in euros") +
      ggtitle("Imported raw materials prices index in France")
    
    
  }else{
    
    gg =
      ggplot(data, aes(x = DATE, y = OBS_VALUE)) +
      facet_wrap(~TITLE_FR2, scales = "free") +
      geom_line() +
      labs(subtitle = "Prix en euros") +
      ggtitle("Indices des prix des mati\u00E8res premi\u00E8res import\u00E9es en France")
    
    
  }
  
  gg = gg %>% add_style(lang = lang)
  gg = gg + theme(strip.text = element_text(size = 12),
                  axis.text.y = element_text(size = 8))
  return(gg)
}

