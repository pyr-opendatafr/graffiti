
enquete_indus = function(lang = "en"){

  data =
    get_insee_idbank("001585934", "001586057", "001788037", "001788035") %>%
    split_title(lang = "fr") %>%
    split_title(lang = "en")

  gg =
    ggplot(data, aes(x = DATE, y = OBS_VALUE, colour = TITLE_FR3)) +
    geom_line(show.legend = FALSE)

  if(lang == "en"){

    gg =
      gg +
      facet_wrap(~TITLE_EN3, scales = "free") +
      ggtitle("Surveys in the industry")

  }else{

    gg =
      gg +
      facet_wrap(~TITLE_FR3, scales = "free") +
      ggtitle("Enqu\U00EAtes dans l'industrie")

  }

  # add_style function is available on GitHub
  gg = gg %>% add_style(lang = lang)

  return(gg)
}

