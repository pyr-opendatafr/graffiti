
pib_growth_qoq = function(lang = "en"){
  
  data =
    get_insee_idbank("010565692") %>%
    split_title(lang = "fr") %>%
    split_title(lang = "en") %>% 
    filter(DATE >= "1960-01-01")
  
  gg =
    ggplot(data, aes(x = DATE, y = OBS_VALUE)) +
    geom_col(show.legend = FALSE)
  
  if(lang == "en"){
    
    gg =
      gg +
      ggtitle("GDP growth rate quarter-on-quarter")
    
  }else{
    
    gg =
      gg +
      ggtitle("PIB - taux de croissance trimestriel")
    
  }
  
  # add_style function is available on GitHub
  gg = gg %>% add_style(lang = lang)
  
  return(gg)
}

