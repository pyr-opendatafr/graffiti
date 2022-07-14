
immat = function(lang = "en"){
  
  # dataset_list = get_dataset_list()
  
  # idbank_list_selected =
  #   get_idbank_list("TRANSPORTS") %>%
  #   add_insee_title()
  
  immat = get_insee_idbank("001641574", "001641573")
  
  if(lang == "en"){
    gg =
      ggplot(immat, aes(x = DATE, y = OBS_VALUE, colour = TITLE_EN)) +
      geom_line() +
      ggtitle("New passenger cars registrations")
  }else{
    gg =
      ggplot(immat, aes(x = DATE, y = OBS_VALUE, colour = TITLE_FR)) +
      geom_line() +
      ggtitle("Immatriculations de voitures particuli\U00E8res neuves")
  }
  
  gg = gg %>% add_style(lang = lang)
  gg = gg + guides(colour = guide_legend(ncol = 1))
  
  return(gg)
}

