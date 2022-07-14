
pop_map = function(lang = "en"){
  
  pkg_needed = c("maptools", "raster", "rgdal", "geosphere", "broom", "viridis", "mapproj")
  
  pkg_installed = installed.packages()
  
  pkg_missing = pkg_needed[!pkg_needed %in% pkg_installed]
  
  if(length(pkg_missing) > 0){
    gg = ggplot() + ggtitle(sprintf("To get the map please download the following packages : %s", paste0(pkg_missing, collapse = " ")))
    return(gg)
  }
  
  library(mapproj)
  
  list_idbank = 
    get_idbank_list("TCRED-ESTIMATIONS-POPULATION") %>%
    filter(AGE == "00-") %>% #all ages
    filter(SEXE == 0) %>% #men and women
    filter(str_detect(REF_AREA, "^D")) 
  
  list_idbank_selected = list_idbank %>% pull(idbank)
  
  # get population data by departement
  pop = get_insee_idbank(list_idbank_selected, lastNObservations = 1) 
  
  #get departements' geographical limits
  FranceMap <- raster::getData(name = "GADM", country = "FRA", level = 2)
  
  # extract the population by departement in 2020
  pop_plot = pop %>%
    group_by(TITLE_EN) %>%
    mutate(dptm = gsub("D", "", REF_AREA)) %>%
    filter(dptm %in% FranceMap@data$CC_2) %>%
    mutate(dptm = factor(dptm, levels = FranceMap@data$CC_2)) %>%
    arrange(dptm) %>%
    mutate(id = dptm)
  
  max_date = pop_plot %>% pull(DATE) %>% max()
  
  vec_pop = pop_plot %>% pull(OBS_VALUE)
  
  # add population data to the departement object map
  FranceMap@data$pop = vec_pop
  
  get_area = function(long, lat){
    area = geosphere::areaPolygon(data.frame(long = long, lat = lat)) / 1000000
    return(data.frame(area = area))
  }
  
  # extract the departements' limits from the spatial object and compute the surface
  FranceMap_tidy_area <- 
    broom::tidy(FranceMap) %>% 
    group_by(id) %>%
    group_modify(~get_area(long = .x$long, lat = .x$lat))
  
  FranceMap_tidy <- 
    broom::tidy(FranceMap) %>% 
    left_join(FranceMap_tidy_area)
  
  # mapping table
  dptm_df = data.frame(dptm = FranceMap@data$CC_2,
                       dptm_name = FranceMap@data$NAME_2,
                       pop = FranceMap@data$pop,
                       id = rownames(FranceMap@data))
  
  FranceMap_tidy_final_all =
    FranceMap_tidy %>%
    left_join(dptm_df, by = "id") %>%
    mutate(pop_density = pop/area) %>% 
    mutate(density_range = case_when(pop_density < 40 ~ "< 40",
                                     pop_density >= 40 & pop_density < 50 ~ "[40, 50]",
                                     pop_density >= 50 & pop_density < 70 ~ "[50, 70]",
                                     pop_density >= 70 & pop_density < 100 ~ "[70, 100]",
                                     pop_density >= 100 & pop_density < 120 ~ "[100, 120]",
                                     pop_density >= 120 & pop_density < 160 ~ "[120, 160]",
                                     pop_density >= 160 & pop_density < 200 ~ "[160, 200]",
                                     pop_density >= 200 & pop_density < 240 ~ "[200, 240]",
                                     pop_density >= 240 & pop_density < 260 ~ "[240, 260]",
                                     pop_density >= 260 & pop_density < 410 ~ "[260, 410]",
                                     pop_density >= 410 & pop_density < 600 ~ "[410, 600]",
                                     pop_density >= 600 & pop_density < 1000 ~ "[600, 1000]",
                                     pop_density >= 5000 & pop_density < 10000 ~ "[5000, 10000]",
                                     pop_density >= 20000 ~ ">= 20000"
    )) %>% 
    mutate(density_range = factor(density_range,
                                                  levels = c("< 40","[40, 50]", "[50, 70]","[70, 100]",
                                                             "[100, 120]", "[120, 160]", "[160, 200]",
                                                             "[200, 240]", "[240, 260]", "[260, 410]",
                                                             "[410, 600]",  "[600, 1000]",
                                                             "[5000, 10000]", ">= 20000")))
  
  
  
  if(lang == "en"){
    
    gg =
      ggplot(data = FranceMap_tidy_final_all,
             aes(fill = density_range, x = long, y = lat, group = group) ,
             size = 0, alpha = 0.9) +
      geom_polygon() +
      geom_path(colour = "white") +
      coord_map() +
      theme_void() +
      viridis::scale_fill_viridis(discrete = T) + 
      theme(legend.title = element_blank()) +
      ggtitle("Distribution of the population within French territory") +
      labs(subtitle = sprintf("The density, in people per square kilometer, displayed here is an approximation, it should not be considered as an official statistics\nYear : %s", lubridate::year(max_date)))
    
    
  }else{
    
    gg =
      ggplot(data = FranceMap_tidy_final_all,
             aes(fill = density_range, x = long, y = lat, group = group) ,
             size = 0, alpha = 0.9) +
      geom_polygon() +
      geom_path(colour = "white") +
      coord_map() +
      theme_void() +
      viridis::scale_fill_viridis(discrete = T) + 
      ggtitle("R\u00E9partition de la population sur le territoire - densit\u00E9 par kilom\u00E8tre carr\u00E9") +
      labs(subtitle = sprintf("Ceci est une approximation et non un chiffre officiel\nAnn\u00E9e: %s", lubridate::year(max_date))) +
      theme(legend.title = element_blank())
    
    
  }
  
  gg = gg %>% add_map_style(lang = lang)
  
  return(gg)
}

