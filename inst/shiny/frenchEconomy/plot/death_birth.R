
death_birth = function(lang = "en"){
  
  # list_idbank_selected =
  #   get_idbank_list("DECES-MORTALITE", "NAISSANCES-FECONDITE") %>%
  #   filter(FREQ == "M") %>% #monthly
  #   filter(REF_AREA == "FM") %>% #metropolitan territory
  #   filter(DEMOGRAPHIE %in% c("NAISS", "DECES"))
  # 
  # idbank_selected = list_idbank_selected %>% pull(idbank)
  
  data =
    get_insee_idbank("000436394", "000436391") %>% 
    split_title() %>%
    mutate(period = case_when(DATE < "1975-01-01" ~ "1948 - 1974",
                              DATE >= "1975-01-01" & DATE < "2000-01-01" ~ "1975 - 1999",
                              DATE >= "2000-01-01" ~ "2000 - today"
    )) %>% 
    mutate(period_fr = gsub("today", "aujourd'hui", period))
  
  x_dates = seq.Date(from = as.Date("1940-01-01"), to = Sys.Date(), by = "5 years")
  
  if(lang == "en"){
    
    gg =
      ggplot(data, aes(x = DATE, y = OBS_VALUE, colour = TITLE_EN2)) +
      facet_wrap(~period, scales = "free_x", ncol = 1) +
      geom_line() +
      geom_point(size = 0.9) +
      ggtitle("Deaths and Births in France since 1948") 
      
    
  }else{
    
    gg =
      ggplot(data, aes(x = DATE, y = OBS_VALUE, colour = TITLE_FR2)) +
      facet_wrap(~period_fr, scales = "free_x", ncol = 1) +
      geom_line() +
      geom_point(size = 0.9) +
      ggtitle("D\u00E9c\U00E8s et naissances en France depuis 1948")
    
  }
  
  # add_style function is available on GitHub
  gg = gg %>% add_style(lang = lang)
  gg = suppressMessages(gg + scale_x_date(breaks = x_dates, date_labels = "%Y"))
  
  return(gg)
}

