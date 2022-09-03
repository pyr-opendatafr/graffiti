#' @importFrom magrittr "%>%"
#' @importFrom rlang ".data"
#' @export
add_style = function(gg, lang = "en",
                     last_update = TRUE,
                     interactive = FALSE){
  
  if(!interactive){
    data = gg[["data"]]
    # idbank_used = data %>% distinct(IDBANK) %>% pull(IDBANK) %>% paste0(collapse = " ")
  }else{
    # how can I get the data back
    data = plotly_data(gg)
    #Print(typeof(data))
    # Print(str(gg))
    # Print(str(gg$x))
  }

  if("DATE" %in% names(data)){
    last_update_date = data %>% pull(DATE) %>% max()
  }
  
  
  if(lang == "en"){
    caption_text_start = "Made with graffiti on"
    # caption_text_series = "Used series"
  }else{
    caption_text_start = "Fait avec graffiti le"
    # caption_text_series = "S\u00E9ries utilis\u00E9es"
  }

  caption_text_added = sprintf("%s : %s, source : INSEE",
                               caption_text_start, lubridate::now()
  )

  # caption_text_added = sprintf("%s : %s, %s : %s",
  #                              caption_text_start, lubridate::now(),
  #                              caption_text_series, idbank_used
  #                              )
  if(!interactive){
    
    if(is.null(gg$labels$caption)){
      caption_text = caption_text_added
    }else{
      caption_text = paste0(gg$labels$caption, "\n", caption_text_added)
    }
  
    gg_new =
      gg +
      ggplot2::scale_x_date(expand = c(0.01, 0.01)) +
      ggplot2::labs(caption = caption_text) +
      ggthemes::theme_stata() +
      ggplot2::theme(
        text = ggplot2::element_text(size = 15),
        axis.text.y  = ggplot2::element_text(angle = 0, hjust = 1),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        plot.caption = ggplot2::element_text(size = 15),
        legend.title = ggplot2::element_blank(),
        legend.position = "bottom"
      )
  
    if("DATE" %in% names(data)){
      if(last_update == TRUE){
        if(lang == "en"){
          subtt = sprintf("Last update : %s", last_update_date)
        }else{
          subtt = sprintf("Derni\U00E8re date : %s", last_update_date)
        }
        
        if(!is.null(gg$labels$subtitle)){
          subtt_final = sprintf("%s\n%s", gg$labels$subtitle, subtt)
        }else{
          subtt_final = subtt
        }
        
        gg_new = gg_new + ggplot2::labs(subtitle = subtt_final)
      }
    }
    
    return(gg_new)
  }else{
    
    if("DATE" %in% names(data)){
      if(last_update == TRUE){
        if(lang == "en"){
          subtt = sprintf("Last update : %s", last_update_date)
        }else{
          subtt = sprintf("Derni\U00E8re date : %s", last_update_date)
        }
        subtt_final = paste0(caption_text_added, ". ", subtt, ".")
      }else{
        subtt_final = paste0(caption_text_added, ".")
      }
    }
    
    gg =
      gg %>%
      plotly::layout(
        annotations = list(x = 0, y = 1.01, yanchor = "auto", #xanchor = "auto",
                           text = subtt_final,
                           showarrow = F, xref='paper', yref='paper')
      )
    
    return(gg)
  }
}

