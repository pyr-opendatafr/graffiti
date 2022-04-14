#' @importFrom magrittr "%>%"
#' @importFrom rlang ".data"
#' @importFrom dplyr "distinct"
#' @importFrom dplyr "pull"
#' @importFrom dplyr "filter"
#' @export
gg_plotly = function(data, lang = "en"){

  idbank_selected = data %>% distinct(.data$IDBANK) %>% pull(.data$IDBANK)

  list_fig = lapply(idbank_selected, function(id, lang_fig = lang){

    data_id = data %>% filter(.data$IDBANK == id)

    if(lang == "en"){
      title_fig = data_id %>% distinct(.data$TITLE_EN) %>% pull(.data$TITLE_EN)
      hovertext_date = "Date :"
      hovertext_value = "<br>Value :"
    }else{
      title_fig = data_id %>% distinct(.data$TITLE_FR) %>% pull(.data$TITLE_FR)
      hovertext_date = "Date :"
      hovertext_value = "<br>Valeur :"
    }

    fig = plotly::plot_ly(data_id, x = ~DATE, y = ~OBS_VALUE, type = 'scatter', mode = "lines",
                          text = ~paste(sprintf("%s", hovertext_date), DATE,
                                        sprintf("%s", hovertext_value), round(OBS_VALUE, 2)),
                          hoverinfo = "text",
                          name = title_fig) %>%
      plotly::layout(legend = list(orientation = 'h')) %>%
      plotly::add_annotations(
        text = title_fig,
        x = 0,
        y = 1,
        yref = "paper",
        xref = "paper",
        xanchor = "left",
        yanchor = "top",
        showarrow = FALSE
      )

    return(fig)
  })

  nrows_fig = floor(sqrt(length(idbank_selected)))

  fig_final = plotly::subplot(list_fig, nrows = nrows_fig)

  return(fig_final)
}


