
air_traffic = function(lang = "en"){
  
  id_transport =
    get_idbank_list('TRANSPORTS') %>%
    add_insee_title() %>%
    filter(!str_detect(title, 'Stopped series')) %>% 
    filter(TYPE_RESEAU == '140') %>%
    filter(UNIT_MEASURE == 'INDIVIDUS') %>% 
    filter(INDICATEUR == 'TRANSPORTS_VOYAGEURS') %>%
    filter(!LOCALISATION %in% c(11:14, 'SO', '19-DOM', '24-DOM'))
  
  idbank_transport_selected = id_transport %>% pull(idbank)
  
  
  order_last_value = function(data, col_to_order,
                              numValCol = "OBS_VALUE", dateCol = "DATE"){
    
    for(col in col_to_order){
      var_order = data %>%
        group_by(!!sym(col)) %>% 
        arrange(desc(!!sym(dateCol))) %>% 
        slice(1) %>% 
        arrange(desc(!!sym(numValCol))) %>% 
        pull(!!sym(col))
      
      data = data %>% 
        mutate(!!sym(col) := factor(!!sym(col), levels = var_order))
    }
    return(data)
  }
  
  data = 
    get_insee_idbank(idbank_transport_selected) %>% 
    split_title() %>% 
    add_insee_metadata() %>% 
    filter(DATE >= '2005-01-01') %>% 
    order_last_value(col_to_order = c('LOCALISATION_label_en', 'LOCALISATION_label_fr'))
  
  
  if(lang == "en"){
    gg =
      ggplot(data, aes(x = DATE, y = OBS_VALUE, colour = LOCALISATION_label_en)) +
      facet_wrap(~LOCALISATION_label_en, scales = 'free', labeller = label_wrap_gen(40)) +
      geom_line(show.legend = F) +
      geom_point(show.legend = F) +
      ggtitle('Internal and international flights') +
      labs(subtitle = 'Unit : number of flights in millions')
  }else{
    gg =
      ggplot(data, aes(x = DATE, y = OBS_VALUE, colour = LOCALISATION_label_fr)) +
      facet_wrap(~LOCALISATION_label_fr, scales = 'free', labeller = label_wrap_gen(40)) +
      geom_line(show.legend = F) +
      geom_point(show.legend = F) +
      ggtitle('Vols nationaux et internationaux') +
      labs(subtitle = 'Unit\u00E9 : nombre de vols en millions')
  }
  
  gg = gg %>% add_style(lang = lang)
  gg = gg + theme(strip.text = element_text(size = 11))
  
  return(gg)
}

