css_idbank_list <- HTML(
  "#idbank_list > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody {
        transform:rotateX(180deg);
    }
    #idbank_list > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody table{
        transform:rotateX(180deg);
    }"
)

css_data_table <- HTML(
  "#data_table > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody {
        transform:rotateX(180deg);
    }
    #data_table > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody table{
        transform:rotateX(180deg);
    }"
)

shinyUI(
  dashboardPagePlus(
    enable_preloader = F,
    dashboardHeaderPlus(
      title = div(img(src="assets/graffiti.png", width="70%"), ""),
                        titleWidth = 285,
                        enable_rightsidebar = FALSE,
                        left_menu = tagList(

                          div(id = "lang_en",
                              img(src = "assets/gb.svg", style="width: 25px", align = "right")
                          ),
                          tags$style(
                            '#lang_en {cursor: pointer}'
                          ),

                          div(id = "lang_fr",
                              img(src = "assets/fr.svg", style="width: 25px", align = "right")
                          ),
                          tags$style(
                            '#lang_fr {cursor: pointer}'
                          )

                        ),
      dropdownMenu(type = "notifications",
                   headerText = '',
                   badgeStatus = "warning",
                   notificationItem(
                     text = htmlOutput("warning_insee_data"),
                     icon = icon("exclamation-triangle"),
                     status = "warning"
                   )
      )
                        ),
    dashboardSidebar(width=285,
                     sidebarMenu(id = "tabs_menu",
                                # menuItem(textOutput("menu_item1", inline = TRUE), tabName = "1",
                                 #         icon = icon("desktop"),startExpanded = F),

                                 menuItem(textOutput("menu_item2", inline = TRUE), tabName = "2",
                                          icon = icon("chart-area"),startExpanded = F),

                                   menuItem(textOutput("menu_item3", inline = TRUE),
                                          menuSubItem(textOutput("export_data", inline = TRUE), tabName = "sub_1"), 
                                          menuSubItem(textOutput("enhance_plot", inline = TRUE), tabName = "sub_2"),
                                          # menuSubItem("Export Data", tabName = "sub_1"), 
                                          # menuSubItem("Enhance your plot", tabName = "sub_2"),
                                          icon = icon("chart-area"),
                                          startExpanded = F),
                                 hidden(menuItem("hiddenCharts", tabName = "hiddenCharts")),
                                 
                                 conditionalPanel(
                                   condition = "input.tabs_menu == 'sub_1' || input.tabs_menu == '2'",
                                   fluidRow(
                                     tags$style(type = "text/css", "#downloadData {color: black;}"),
                                     column(12, align = "center", offset = 0,
                                            downloadButton("downloadData",
                                                           label = textOutput("downloadData_title", inline = TRUE))
                                     )
                                   ),
                                   fluidRow(
                                     column(12, align = "center", offset = 0,
                                            div(style="display: inline-block;vertical-align:bottom; width: 250px;",
                                              uiOutput("slides"))
                                            #  width: 150px;
                                             # ,div(style="display: inline-block;vertical-align:center;width: 5px;margin-top:50px",

                                              # downloadButton("downloadSlides", label = textOutput("slides_download", inline = TRUE))
                                            # )
                                     )
                                   )
                                   ,fluidRow(
                                     tags$style(type = "text/css", "#downloadSlides {color: black;}"),
                                     column(12, align = "center", offset = 0,
                                            downloadButton("downloadSlides", label = textOutput("slides_download", inline = TRUE))
                                     )
                                   )
                                 ) #cond panel 2
                                 ,

                                 conditionalPanel(
                                   condition = "input.tabs_menu == 'sub_2'", #"input.tabs_menu == '3'"
                                   fluidRow(
                                     column(12, align = "center", offset = 0,
                                   actionBttn(
                                     inputId = "new_plot",
                                     label = textOutput("new_plot_title", inline = TRUE),
                                     style = "gradient",
                                     color = "succes",
                                     icon = icon("sync")
                                   )
                                     )),
                                   fluidRow(
                                     column(12, align = "center", offset = 0,
                                           actionButton(inputId = "deselect_idbank_in_list",
                                                        label = textOutput("deselect_idbank_in_list_title",
                                                                           inline = TRUE))
                                     )
                                   ),
                               fluidRow(
                                 column(12, align = "center", offset = 0,
                               switchInput(
                                 inputId = "interactive_plot",
                                 label = textOutput("interactive_plot_title", inline = TRUE),
                                 labelWidth = "150px",
                                 onStatus = "success",
                                 offStatus = "danger"
                               )
                                 )
                               ),
                               fluidRow(
                                 column(12, align = "center", offset = 0,
                                        switchInput(
                                          inputId = "one_plot",
                                          label = textOutput("one_plot_title", inline = TRUE),
                                          labelWidth = "150px",
                                          onStatus = "success",
                                          offStatus = "danger"
                                        )
                                 )
                               ),
                               fluidRow(
                                 column(12, align = "center", offset = 0,
                                          uiOutput("slider_period")
                                        )
                               ),
                               fluidRow(
                                 column(12,align = "left", offset = 0,
                                       uiOutput("growth_button")
                                        )
                               )
                                 ) #cond panel 3
                     )
    ),
    ## Body content
    dashboardBody(
      useShinyjs(),
      tags$style(css_idbank_list),
      tags$style(css_data_table),
      tabItems(
        tabItem(tabName = "2",
                fluidRow(fillCol(
                  tags$style(
                    '#list_tab {cursor: pointer}'
                  ),
                  uiOutput("list_tab")
                  ))),
        tabItem(tabName = "hiddenCharts",
                fluidRow(
                  column(9,
                         # tags$style(type = "text/css", ".dataset_picker {background-color:white}"),
                uiOutput("dataset_picker")
                )
                ,column(3,
                uiOutput("idbank_picker")
                  )
                ),
                fluidRow(
                  uiOutput("dims")),
                tags$style(
                  '#idbank_list {cursor: pointer}'
                ),
                fluidRow(
                uiOutput("list_tab2"))
                )
      )
    )
  ))


