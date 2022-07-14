
rdb_pvr_achat = function(lang = "en"){
  
  # idbank_list = 
  #   get_idbank_list("CNA-2014-RDB") 
  # 
  # idbank_selected = idbank_list %>% pull(idbank)
  # 
  
  data = get_insee_idbank("010562201", "010562206", "010562202", "010562204", "010562203", 
                          "010562205", "010562212",
                          "010562211", "010562207", "010562209", "010562208", "010562210") %>% 
    add_insee_metadata()
  
  if(lang == "en"){
    
    gg =
      ggplot(data, aes(x = DATE, y = OBS_VALUE)) +
      geom_line() +
      geom_point() +
      facet_wrap(~INDICATEUR_label_en, scales = "free", labeller = label_wrap_gen(40)) +
      ggtitle("Gross disposable income of households - purchasing power")
    
    
  }else{
    
    gg =
      ggplot(data, aes(x = DATE, y = OBS_VALUE)) +
      geom_line() +
      geom_point() +
      facet_wrap(~INDICATEUR_label_fr, scales = "free", labeller = label_wrap_gen(40)) +
      ggtitle("Revenu disponible brut des m\u00E9nages - pouvoir d'achat")
    
  }
  
  gg = gg %>% add_style(lang = lang)
  gg = gg + theme(strip.text = element_text(size = 12))
    
  return(gg)
}

