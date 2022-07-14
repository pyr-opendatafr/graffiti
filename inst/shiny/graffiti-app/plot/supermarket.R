
supermarket = function(lang = "en"){
  
  # idbank_list =
  #   get_idbank_list("ICA-2015-EMAGSA") 
  # 
  # idbank_selected = idbank_list %>% pull(idbank)
  
  data =
    get_insee_idbank("010545333", "010545334", "010545345", "010545346", "010545339", "010545340", "010545337", "010545338",
                     "010545347", "010545348", "010545343", "010545344", "010545331",
                     "010545332", "010545335", "010545336", "010545349", "010545350",
                     "010545341", "010545342", "010545329", "010545330", "010545327", "010545328") %>% 
    split_title()
  if(lang == "en"){
    
    gg =
      ggplot(data, aes(x = DATE, y = OBS_VALUE)) +
      facet_wrap(~TITLE_EN2, scales = "free", labeller = label_wrap_gen(30)) +
      geom_line() +
      ggtitle("Sales in supermarkets")
    
  }else{
    
    gg =
      ggplot(data, aes(x = DATE, y = OBS_VALUE)) +
      facet_wrap(~TITLE_FR2, scales = "free", labeller = label_wrap_gen(30)) +
      geom_line() +
      ggtitle("Chiffres d'affaires des enseignes de grande distribution")
    
  }
  
  gg = gg %>% add_style(lang = lang)
  
  gg = gg + theme(strip.text = element_text(size = 12))
  
  return(gg)
}

