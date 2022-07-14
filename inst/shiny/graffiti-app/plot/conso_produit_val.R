
conso_produit_val = function(lang = "en"){
  
  # dt = get_dataset_list()
  
  # Final household consumption expenditure consists of expenditure incurred by resident
  # household on goods or services that are used for the satisfaction of needs or wants.
  # The corresponding products are not stored, but are considered as consumed at the time
  # of their purchase, even if they are durable goods
  # (cars, household electrical appliances, furniture, etc.).
  # Final household consumption expenditure includes the share of expenses on health,
  # education and housing remaining to be paid by them, after possible reimbursements.
  # It also includes imputed rents, which households that own their residence implicitly
  # pay to themselves.
  
  # Actual final consumption of households includes all goods and services acquired by
  # resident households, for the satisfaction of their needs and wants, and that these
  # acquisitions made, or not, object of an expense of their part.
  # Actual final consumption of households thus includes, besides goods and services
  # acquired by their own  final household consumption expenditure, goods and services which,
  # having been the object of actual individual consumption expenses of general government or
  # NPISHs, give rise to social transfers in kind receivable towards households.
  
  idbank_list = 
    get_idbank_list("CNA-2014-CONSO-MEN") %>%
    filter(str_detect(INDICATEUR, "^CNA_CONSO_MENAGES_FONCTION")) %>%
    filter(PRIX_REF == "VAL") %>%
    filter(nchar(CNA_PRODUIT) == 5)
  
  idbank_list_tot =
    get_idbank_list("CNA-2014-CONSO-MEN") %>%
    filter(str_detect(INDICATEUR, "^CNA_CONSO_MENAGES_FONCTION")) %>%
    filter(PRIX_REF == "VAL") %>%
    filter(CNA_PRODUIT %in% c("FONTOTAL")) #"FONDEP"
  
  idbank_selected = idbank_list %>% pull(idbank)
  idbank_selected_tot = idbank_list_tot %>% pull(idbank)
  
  start_date_plot = as.Date("1980-01-01")
  
  x_scale = seq.Date(from = start_date_plot, to = lubridate::today(), by = "5 years")
  
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
    get_insee_idbank(idbank_selected) %>%
    split_title(pattern = "-") %>% 
    add_insee_metadata() %>% 
    filter(DATE >= start_date_plot) %>% 
    order_last_value(col_to_order = c('TITLE_FR4', 'TITLE_EN4'))
  
  conso_tot = 
    get_insee_idbank(idbank_selected_tot) %>% 
    filter(DATE >= start_date_plot) %>% 
    split_title(pattern = "-", n_split = 4)
  
  colors_ = c(RColorBrewer::brewer.pal(7,'Set1'), RColorBrewer::brewer.pal(8,'Set2'),
              RColorBrewer::brewer.pal(11,'Set3'),
              RColorBrewer::brewer.pal(9, 'Pastel1'), RColorBrewer::brewer.pal(8, 'Pastel2'),
              RColorBrewer::brewer.pal(8, 'Dark2'), RColorBrewer::brewer.pal(12, 'Paired'))
  
  if(lang == "en"){
    
    gg =
      ggplot(data = data) +
      geom_col(data = data, aes(x = DATE, y = OBS_VALUE, fill = TITLE_EN4)) +
      geom_point(data = conso_tot, aes(x = DATE, y = OBS_VALUE, colour = TITLE_EN4)) +
      scale_fill_manual(values = colors_) +
      scale_colour_manual(values = "black") +
      guides(fill = guide_legend(ncol = 3)) +
      ggtitle("Actual household consumption - current price")
    
  }else{
    
    gg =
      ggplot(data = data) +
      geom_col(data = data, aes(x = DATE, y = OBS_VALUE, fill = TITLE_FR4)) +
      geom_point(data = conso_tot, aes(x = DATE, y = OBS_VALUE, colour = TITLE_FR4)) +
      scale_fill_manual(values = colors_) +
      scale_colour_manual(values = "black") +
      guides(fill = guide_legend(ncol = 3)) +
      ggtitle("Consommation effective des m\u00E9nages - prix courant")
  }
  
  gg = gg %>% add_style(lang = lang)
  
  gg = gg + 
    theme(legend.text = element_text(size = 11)) +
    suppressMessages(scale_x_date(breaks = x_scale,
                                  date_labels = "%Y", expand = c(0.01, 0.01)))
  
  return(gg)
}

