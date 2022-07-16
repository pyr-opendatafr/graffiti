#' @importFrom magrittr "%>%"
#' @export
gg_plotly_one_plot = function(data, lang = "en"){
  
  MIN_DATE = min(data$DATE)
  MAX_DATE = max(data$DATE)
  xlabel = ''
  ylabel = unique(data$UNIT_MEASURE)
  if (length(ylabel) != 1) { ylabel = '' }
  
  if(lang == "en"){
    hovertext_date = "Date :"
    hovertext_value = "<br>Value :"
    NAME = "TITLE_EN"
    button_label_3_m = "3 months"
    button_label_6_m = "6 months"
    button_label_1_y = "1 year"
    button_label_3_y = "3 years"
    button_label_10_y = "10 years"
    button_label_ytd = "year to date"
    button_label_all = "all"
  }else{
    hovertext_date = "Date :"
    hovertext_value = "<br>Valeur :"
    NAME = "TITLE_FR"
    button_label_3_m = "3 mois"
    button_label_6_m = "6 mois"
    button_label_1_y = "1 an"
    button_label_3_y = "3 ans"
    button_label_10_y = "10 ans"
    button_label_ytd = "ann\u00E9e civile"
    button_label_all = "tout"
  }
  
  fig <- 
    plotly::plot_ly(data, x = ~DATE, y = ~OBS_VALUE, color = ~IDBANK,
                    type = 'scatter', mode = "lines",
                    text = ~paste(sprintf("%s", hovertext_date), DATE,
                                  sprintf("%s", hovertext_value), round(OBS_VALUE, 2)),
                    hoverinfo = "text",
                    name = ~get(NAME)) %>%
    plotly::config(displaylogo = FALSE, locale = lang) %>%
    plotly::layout(
      legend = list(orientation = 'h', x=0, y=-0.22, xanchor = "left", yanchor = "top"),
      xaxis = list(title = xlabel,
                   rangeslider = list(borderwidth = 1, thickness = 0.09,
                                      start = MIN_DATE, end = MAX_DATE),
                   tickformat = "%b %Y",
                   ticklabelmode="period",
                   rangeselector = list(
                     buttons = list(
                       list(count = 3, label = button_label_3_m, step = "month", stepmode = "backward"),
                       list(count = 6, label = button_label_6_m, step = "month", stepmode = "backward"),
                       list(count = 1, label = button_label_1_y, step = "year", stepmode = "backward"),
                       list(count = 3, label = button_label_3_y, step = "year", stepmode = "backward"),
                       list(count = 10, label = button_label_10_y, step = "year", stepmode = "backward"),
                       list(count = 1, label = button_label_ytd, step = "year", stepmode = "todate"),
                       list(step = "all", label = button_label_all))
                   ),
                   yaxis = list(rangemode = "auto"),
                   type = "date"
      ),
      yaxis = list(title = ylabel, autorange = T, fixedrange = F)
    )
  return(fig)
}
