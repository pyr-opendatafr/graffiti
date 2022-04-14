
survey_household = function(lang = "en"){
  
  # idbank_list =
  #   get_idbank_list() %>%
  #   filter(str_detect(nomflow, "ENQ.*MENAGE")) %>%
  #   add_insee_title(lang = "fr") %>%
  #   filter(dim7 == "CVS")
  # 
  # idbank_selected = idbank_list %>% pull(idbank)
  
  get_string_left = function(x, pattern = "\\:|\\("){
    points_loc = min(unique(stringr::str_locate(x, pattern)[[1]]))
    if(is.na(points_loc)){
      return(x)
    }
    string = gsub("\\s$|\\:|\\(", "", substr(x, 1, points_loc))
    return(string)
  }
  
  data =
    get_insee_idbank("000857195", "000857190", "000857189", "000857188",
                     "000857191", "000857197", "000857192", "000857198",
                     "000857193", "000857194", "001587668",
                     "001616792", "001616793", "001616794", "001616795") %>%
    split_title() %>%
    mutate(TITLE_FR2_short = map_chr(TITLE_FR2, get_string_left)) %>% 
    mutate(TITLE_EN2_short = map_chr(TITLE_EN2, get_string_left)) 
  
  if(lang == "en"){
    
    gg =
      ggplot(data, aes(x = DATE, y = OBS_VALUE)) +
      facet_wrap(~TITLE_EN2_short, scales = "free", ncol = 3) +
      geom_line() +
      labs(subtitle = "Balance of responses") +
      ggtitle("Households' survey about economic outlook") 
    
  }else{
    
    gg =
      ggplot(data, aes(x = DATE, y = OBS_VALUE)) +
      facet_wrap(~TITLE_FR2_short, scales = "free", ncol = 3) +
      geom_line() +
      labs(subtitle = "Soldes d'opinion") +
      ggtitle("Enqu\U00EAtes de conjoncture au pr\u00E8s des m\u00E9nages")
    
  }
  
  gg = gg %>% add_style(lang = lang)
  
  gg = gg + theme(strip.text = element_text(size = 12),
                  axis.text.y = element_text(size = 8))
  return(gg)
}

