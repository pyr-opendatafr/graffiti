#' @importFrom magrittr "%>%"
#' @importFrom rlang ".data"
#' @export
add_map_style = function(gg, lang = "en",
                     last_update = TRUE,
                     layout){

  data = gg[["data"]]
  # idbank_used = data %>% distinct(IDBANK) %>% pull(IDBANK) %>% paste0(collapse = " ")

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

  if(is.null(gg$labels$caption)){
    caption_text = caption_text_added
  }else{
    caption_text = paste0(gg$labels$caption, "\n", caption_text_added)
  }

  gg_new =
    gg +
    ggplot2::labs(caption = caption_text) 

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
}

