
elect_map = function(lang = "en"){
  
  pkg_needed = c("maptools", "raster", "rgdal", "geosphere", "broom", "viridis")
  
  pkg_installed = installed.packages()
  
  pkg_missing = pkg_needed[!pkg_needed %in% pkg_installed]
  
  if(length(pkg_missing) > 0){
    gg = ggplot() + ggtitle(sprintf("To get the map please download the following packages : %s", paste0(pkg_missing, collapse = " ")))
    return(gg)
  }
  
  idbank_list = 
    get_idbank_list("TCRED-CONDITIONSDEVIE-TXP-CDE") %>% 
    filter(INDICATEUR == "TAUX_PARTICIPATION_PRESIDENTIELLE") %>% 
    filter(str_detect(REF_AREA, "^D"))
  
  idbank_selected = idbank_list %>% pull(idbank)
  
  data = get_insee_idbank(idbank_selected, lastNObservations = 1)
  
  max_date = data %>% pull(DATE) %>% max()
  
  get_dpt_map_data = function(data){
    
    data = data %>% filter(str_detect(REF_AREA, "^D"))
    
    if(nrow(data) == 0){
      warning("missing data on departement")
      return(NULL)
    }
    
    FranceMap <- raster::getData(name = "GADM", country = "FRA", level = 2)
    
    data_plot = data %>%
      group_by(TITLE_EN) %>%
      mutate(dptm = gsub("D", "", REF_AREA)) %>%
      filter(dptm %in% FranceMap@data$CC_2) %>%
      mutate(dptm = factor(dptm, levels = FranceMap@data$CC_2)) %>%
      arrange(dptm) %>%
      mutate(id = dptm)
    
    vec_value = data_plot %>% pull(OBS_VALUE)
    FranceMap@data$value = vec_value
    
    FranceMap_tidy <- 
      broom::tidy(FranceMap)
    
    dptm_df = data.frame(dptm = FranceMap@data$CC_2,
                         dptm_name = FranceMap@data$NAME_2,
                         value = FranceMap@data$value,
                         id = rownames(FranceMap@data))
    
    data_map =
      FranceMap_tidy %>%
      dplyr::left_join(dptm_df, by = "id")
    
    return(data_map)
  }
  
  data_map = get_dpt_map_data(data)
  
  
  if(lang == "en"){
    
    gg =
      ggplot(data = data_map,
             aes(fill = value, x = long, y = lat, group = group) ,
             size = 0, alpha = 0.9) +
      geom_polygon() +
      geom_path(colour = "white") +
      coord_map() +
      theme_void() +
      viridis::scale_fill_viridis() + 
      theme(legend.title = element_blank()) +
      ggtitle("Rate of participation to the last presidential election - first round") +
      labs(subtitle = sprintf("Year : %s", lubridate::year(max_date)))
    
    
  }else{
    
    gg =
      ggplot(data = data_map,
             aes(fill = value, x = long, y = lat, group = group) ,
             size = 0, alpha = 0.9) +
      geom_polygon() +
      geom_path(colour = "white") +
      coord_map() +
      theme_void() +
      viridis::scale_fill_viridis() + 
      theme(legend.title = element_blank()) +
      ggtitle("Taux de participation \U00E0 la derni\u00E8re \u00E9lection pr\u00E9sidentielle - premier tour") +
      labs(subtitle = sprintf("Ann\u00E9e : %s", lubridate::year(max_date)))
    
    
  }
  
  gg = gg %>% add_map_style(lang = lang, last_update = FALSE)
  
  return(gg)
}

