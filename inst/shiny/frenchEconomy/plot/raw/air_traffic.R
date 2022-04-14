


id_transport =
  get_idbank_list('TRANSPORTS') %>%
  add_insee_title() %>%
  filter(!str_detect(title, 'Stopped series')) %>% 
  filter(TYPE_RESEAU == '140') %>%
  filter(UNIT_MEASURE == 'INDIVIDUS') %>% 
  filter(INDICATEUR == 'TRANSPORTS_VOYAGEURS') %>%
  filter(!LOCALISATION %in% c(11:14, 'SO', '19-DOM'))

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
  filter(DATE >= '2000-01-01') %>% 
  order_last_value(col_to_order = c('TITLE_FR3', 'TITLE_EN3'))



ggplot(data, aes(x = DATE, y = OBS_VALUE, colour = TITLE_EN3)) +
  facet_wrap(~TITLE_EN3, scales = 'free') +
  geom_line(show.legend = F) +
  geom_point(show.legend = F) +
  theme(legend.position = 'bottom') +
  ggtitle('Vols nationaux et internationaux')
