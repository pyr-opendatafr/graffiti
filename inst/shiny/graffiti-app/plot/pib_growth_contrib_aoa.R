
pib_growth_contrib_aoa = function(lang = "en"){
  
  
  data_contrib =
    get_insee_idbank("010548515", "010548504", "010548518",
                     "010548519", "010548508", "010548514", "010548520", "010548513") %>%
    split_title(n_split = 4) %>%
    add_insee_metadata() %>% 
    mutate(TITLE_FR_adj = case_when(OPERATION != "P3" ~ paste(OPERATION, "-", TITLE_FR2),
                                    TRUE ~ as.character(paste(OPERATION, "-", TITLE_FR2, "-", TITLE_FR3))
    )) %>% 
    mutate(TITLE_EN_adj = case_when(OPERATION != "P3" ~ paste(OPERATION, "-", TITLE_EN2),
                                    TRUE ~ as.character(paste(OPERATION, "-", TITLE_EN2, "-", TITLE_EN3))
    ))
  
  
  pib =
    get_insee_idbank("010548499") %>%
    split_title(n_split = 2) %>%
    mutate(TITLE_FR = TITLE_FR2) %>%
    mutate(TITLE_EN = TITLE_EN2) %>%
    arrange(DATE) %>%
    mutate(OBS_VALUE = 100 * (OBS_VALUE/dplyr::lag(OBS_VALUE, 1) - 1))
  
  pib_short = pib %>%
    mutate(pib = OBS_VALUE) %>%
    select(DATE, pib)
  
  contrib = data_contrib %>%
    group_by(DATE) %>%
    summarise(contrib = sum(OBS_VALUE)) %>%
    filter(!is.na(contrib)) %>% 
    select(DATE, contrib) %>%
    left_join(pib_short) %>%
    mutate(check = round(pib - contrib, 3))
  
  start_date_plot = as.Date("1950-01-01")
  
  pib_plot = pib %>%
    dplyr::filter(DATE >= start_date_plot)
  
  
  colors_ = c(RColorBrewer::brewer.pal(11, 'Spectral')[2], 
              RColorBrewer::brewer.pal(9, 'Pastel1')[1], 
              RColorBrewer::brewer.pal(7,'Set1')[1],
              RColorBrewer::brewer.pal(7,'Set1')[-1], 
              RColorBrewer::brewer.pal(8,'Set2'))
  
  dates_scale = seq.Date(from = start_date_plot, to = as.Date(paste0(lubridate::year(lubridate::today()), "-01-01")), by = "5 years")
  
  
  if(lang == "en"){
    
    gg =
      ggplot(data = data_contrib) +
      geom_col(data = data_contrib, aes(x = DATE, y = OBS_VALUE, fill = TITLE_EN_adj)) +
      geom_point(data = pib_plot,  aes(x = DATE, y = OBS_VALUE, colour = TITLE_EN)) +
      geom_line(data = pib_plot,  aes(x = DATE, y = OBS_VALUE, colour = TITLE_EN)) +
      ggthemes::theme_stata() +
      guides(fill = guide_legend(ncol = 2)) +
      scale_x_date(expand = c(0.01, 0.01), breaks = dates_scale, date_labels = "%Y") +
      scale_colour_manual(values = "black") +
      scale_fill_manual(values = colors_) +
      ggtitle("GDP - growth contriutions year-over-year, percentage") +
      theme(
        plot.subtitle = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(angle = 0),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )
    
  }else{
    
    gg =
      ggplot(data = data_contrib) +
      geom_col(data = data_contrib, aes(x = DATE, y = OBS_VALUE, fill = TITLE_FR_adj)) +
      geom_point(data = pib_plot,  aes(x = DATE, y = OBS_VALUE, colour = TITLE_FR)) +
      geom_line(data = pib_plot,  aes(x = DATE, y = OBS_VALUE, colour = TITLE_FR)) +
      ggthemes::theme_stata() +
      guides(fill = guide_legend(ncol = 2)) +
      scale_x_date(expand = c(0.01, 0.01), breaks = dates_scale, date_labels = "%Y") +
      scale_colour_manual(values = "black") +
      scale_fill_manual(values = colors_) +
      ggtitle("PIB - contribution des composantes \U00E0 l'\u00E9volution annuelle en %") +
      theme(
        plot.subtitle = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(angle = 0),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )
    
  }
  
  # add_style function is available on GitHub
  gg = gg %>% add_style(lang = lang)
  
  return(gg)
}

