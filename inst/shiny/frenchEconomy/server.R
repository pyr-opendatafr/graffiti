shinyServer(function(input, output, session) {

  lang  <- reactiveVal()
  gg_current_name <- reactiveVal()
  list_plot_selected <- reactiveValues()
  plot_table_react <- reactiveVal()
  dataset_table_react <- reactiveVal()
  idbank_list_react <- reactiveVal()
  idbank_list_view <- reactiveVal()
  get_interactive_plot <- reactiveVal()
  get_one_plot <- reactiveVal()
  data_plot <- reactiveVal()
 
  observeEvent(input$sidebarItemExpanded, {
    # if(input$sidebarItemExpanded != ""){
    #   updateTabItems(session, "tabs", selected = "hiddenCharts")
    # }
    if(!is.null(input$sidebarItemExpanded)){
      updateTabItems(session, "tabs_menu", selected = "hiddenCharts")
    }else{
      # shinyjs::hide(selector = "ul.menu-open")
    }
  })
  
  observe({
    get_interactive_plot(FALSE)
    if(!is.null(input$interactive_plot)){
      if(input$interactive_plot == TRUE){
        get_interactive_plot(TRUE)
      }
    }
  })
  observe({
    get_one_plot(FALSE)
    if(!is.null(input$one_plot)){
      if(input$one_plot == TRUE){
        get_one_plot(TRUE)
      }
    }
  })

  onclick("lang_fr", lang("fr"))
  onclick("lang_en", lang("en"))

  output$lang <- renderPrint({lang()})

  observe({
    if(is.null(lang())){lang_selected = "fr"}else{lang_selected = lang()}

    output$enhance_plot <- renderText(get_label("enhance_plot", lang = lang_selected))
    output$export_data <- renderText(get_label("export_data", lang = lang_selected))
    
    output$dshb_title <- renderText(get_label("dshb_title", lang = lang_selected))
    output$menu_item1 <- renderText(get_label("menu_item1", lang = lang_selected))
    output$menu_item2 <- renderText(get_label("menu_item2", lang = lang_selected))
    output$menu_item3 <- renderText(get_label("menu_item3", lang = lang_selected))

    output$slides <- renderText(get_label("slides_title", lang = lang_selected))
    output$slides_placeholder <- renderText(get_label("slides_placeholder", lang = lang_selected))
    output$slides_download <- renderText(get_label("slides_download", lang = lang_selected))

    output$interactive_plot_title <- renderText(get_label("interactive_plot_title", lang = lang_selected))
    output$new_plot_title <- renderText(get_label("new_plot_title", lang = lang_selected))

    output$dataset_placeholder <- renderText(get_label("dataset_placeholder", lang = lang_selected))
    output$downloadData_title <- renderText(get_label("downloadData_title", lang = lang_selected))
    output$one_plot_title <- renderText(get_label("one_plot_title", lang = lang_selected))

    output$growth_button_title <- renderText(get_label("growth_button_title", lang = lang_selected))
    output$growth_button_raw <- renderText(get_label("growth_button_raw", lang = lang_selected))
    output$growth_button_yoy <- renderText(get_label("growth_button_yoy", lang = lang_selected))
    output$growth_button_pop <- renderText(get_label("growth_button_pop", lang = lang_selected))

    output$deselect_idbank_in_list_title <- renderText(get_label("deselect_idbank_in_list_title", lang = lang_selected))

    output$warning_insee_data <- renderUI(
     tags$div(get_label("warning_insee_data1", lang = lang_selected),
                      tags$br(),
              get_label("warning_insee_data2", lang = lang_selected),
                      style = "display: inline-block; vertical-align: middle;")) 
        
    
    })

  observe({
    if(is.null(lang())){lang_selected = "fr"}else{lang_selected = lang()}

    #
    # PLOT CATALOGUE
    #

    if(lang_selected == "en"){

      plot_table_shown =
        plot_table %>%
        dplyr::select(id, title_en) %>%
        dplyr::rename(Title = title_en)

    }else{

      plot_table_shown =
        plot_table %>%
        dplyr::select(id, title_fr) %>%
        dplyr::rename(Titre = title_fr)

    }

    plot_table_react(plot_table_shown)
    
    plot_table_shown2 = 
      plot_table_shown %>% 
      DT::datatable(
        filter = "none",
        options = list(pageLength = 100),
        rownames = FALSE
      ) %>%
      DT::formatStyle(0, lineHeight = '15px', target = 'row')
    
    
    output$catalogue <- DT::renderDT(plot_table_shown2,  rownames = FALSE)

    list_tab = list()

    list_tab[[length(list_tab)+1]] =
      tabPanel(title = "Catalogue",
               box(
                 width = "100%",
                 DT::dataTableOutput("catalogue", width = "100%", height = "75vh")
               ))

    output$list_tab <- renderUI({
      do.call(tabsetPanel, c(list_tab, id = 'tabs'))
    })

    })

  #
  # IDBANK LIST
  #


observeEvent({
  lang()
},{
  # Print(lang())
  
  if(is.null(lang())){lang_selected = "fr"}else{lang_selected = lang()}

  if(lang_selected == "en"){
    dataset_list_selectize = dataset_list_selectize_en
    dataset_placeholder = "Choose a dataset"
    deselect_all_text = "Deselect all"
    select_all_text = "Select all"

    idbank_list_selected = id_en
    idbank_list_all_label = idbank_list_all_label_en
    idbank_placeholder = "Choose an idbank"

  }else{
    dataset_list_selectize = dataset_list_selectize_fr
    dataset_placeholder = "Choix du jeu de donn\u00E9es"
    deselect_all_text = "Tout d\u00E9s\u00E9lectionner"
    select_all_text = "Tout s\u00E9lectionner"

    idbank_list_selected = id_fr
    idbank_list_all_label = idbank_list_all_label_fr
    idbank_placeholder = "Choix d'une s\u00E9rie"
  }

  # Print(input$growth_button)
  if(!is.null(input$growth_button)){

    updatePrettyRadioButtons(
      session = session,
      inputId = "growth_button",
      selected = input$growth_button,
      label = get_label("growth_button_title", lang = lang_selected),
      choiceValues = c("raw", "yoy", "pop"),
      choiceNames = c(get_label("growth_button_raw", lang = lang_selected),
                  get_label("growth_button_yoy", lang = lang_selected),
                  get_label("growth_button_pop", lang = lang_selected))
    )
  }


    updateSliderInput(session = session,
                      inputId = "slider_period",
                      value = input$slider_period,
                      min = input$slider_period[1],
                      max = input$slider_period[2],
                      label = get_label("slider_period", lang = lang_selected))

  output$dataset_picker <-
    renderUI(
      
      shinyWidgets::pickerInput(
        inputId = "dataset_picker",
        label = NULL,
        choices = dataset_list_id,
        width = "100%",
        multiple = TRUE,
        options =
          list(
            `live-search` = TRUE,
            `actions-box` = TRUE,
            `deselect-all-text` = deselect_all_text,
            `select-all-text` = select_all_text,
            title = dataset_placeholder
          ),
        choicesOpt = list(content = dataset_list_selectize)
      )
      )

  output$idbank_picker <-
    renderUI(
      selectizeInput(
                     inputId = "idbank_picker",
                    label = NULL,
                    choices = NULL,
                    width = "100%",
                    multiple = TRUE,
                    options = list(
                      'plugins' = list('remove_button'),
                      'create' = TRUE,
                      'persist' = FALSE,
                      placeholder = idbank_placeholder,
                      onInitialize = I('function() { this.setValue(""); }')
                    )
      )
      )


  idbank_list_selected =
    idbank_list_selected %>%
    select(nomflow, idbank, title, cleFlow) %>%
    insee::clean_table() %>%
    as.data.frame()

  idbank_list_shown = idbank_list_selected %>%
    DT::datatable(filter = "none", selection = c("multiple"),
                  options = list(pageLength = 100, scrollX = TRUE,
                                 autoWidth = TRUE,
                                 columnDefs = list(list(width = '170px', targets = c(0)),
                                                   list(width = '130px', targets = c(1)),
                                                   list(width = '500px', targets = c(2)))), rownames = FALSE) %>%
    DT::formatStyle(0, lineHeight = '15px', target= 'row')

  idbank_list_react(idbank_list_selected)
  idbank_list_view(idbank_list_selected)
  
  output$idbank_list <- DT::renderDT(idbank_list_shown)

  list_tab = list()

  list_tab[[length(list_tab)+1]] =
    tabPanel(title = get_label("idbank_list", lang = lang_selected),
             box(
               width = "100%",
               DT::dataTableOutput("idbank_list", width = "100%", height = "75vh")
             ))

    output$list_tab2 <- renderUI({
      do.call(tabsetPanel, c(list_tab, id = 'tabs2'))
    })

    output$slides <- renderUI(
      selectizeInput(
        'slides',
        label = get_label("slides_title", lang = lang_selected),
        size = 30,
        width = "300px",
        multiple = TRUE,
        choices = NULL,
        options = list(
          'plugins' = list('remove_button'),
          'create' = TRUE,
          'persist' = FALSE,
          placeholder = get_label("slides_placeholder", lang = lang_selected),
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    )
}, ignoreNULL = FALSE, ignoreInit = FALSE)

observeEvent({
  input$dataset_picker
  },{

  if(is.null(lang())){lang_selected = "fr"}else{lang_selected = lang()}

  if(lang_selected == "en"){
    idbank_list_selected = id_en
    dim_selectize_placeholder = "Filter"
    deselect_all_text = "Deselect all"
    select_all_text = "Select all"
    
  }else{
    idbank_list_selected = id_fr
    dim_selectize_placeholder = "Filtrer"
    deselect_all_text = "Tout d\u00E9s\u00E9lectionner"
    select_all_text = "Tout s\u00E9lectionner"
  }

  idbank_list_selected = idbank_list_selected %>%
    select(nomflow, idbank, title, cleFlow)

  if(!is.null(input$dataset_picker)){

    loc_first_space = stringr::str_locate(input$dataset_picker, "\\s")[1]

    if(is.na(loc_first_space)){
      loc_first_space = nchar(input$dataset_picker)
      }else{
      loc_first_space = loc_first_space - 1
    }

    dataset_selected_name = substr(input$dataset_picker, 1, loc_first_space)

    
    idbank_list_from_dataset =
      insee::get_idbank_list(dataset_selected_name) %>%
      pull(idbank)

    updateSelectizeInput(session, 'idbank_picker',
                         choices = idbank_list_from_dataset,
                         server = TRUE)


    idbank_list_selected =
      insee::get_idbank_list(dataset_selected_name) %>% 
      insee::clean_table()
    
    if(lang_selected == "en"){
      
      id_en_short = id_en %>%
        filter(nomflow %in% dataset_selected_name) %>% 
        select(idbank, title)
      
      idbank_list_selected = idbank_list_selected %>% left_join(id_en_short, by = "idbank")
    }else{
      
      id_fr_short = id_fr %>%
        filter(nomflow %in% dataset_selected_name) %>% 
        select(idbank, title)
      
      idbank_list_selected = idbank_list_selected %>% left_join(id_fr_short, by = "idbank")
    }
    
    first_col = c("nomflow", "idbank", 'title')
    other_col = names(idbank_list_selected)[!names(idbank_list_selected) %in% first_col]
    other_col = other_col[!stringr::str_detect(other_col, "^dim[:digit:]{1,}$")]
    idbank_list_selected = idbank_list_selected[,c(first_col, other_col)]

    # list_dim_column = names(idbank_list_selected)[grep("^dim", names(idbank_list_selected))]
    list_idbank_column = names(idbank_list_selected)
    list_dim_column = list_idbank_column[!stringr::str_detect(list_idbank_column, "^idbank|^cleFlow|^nomflow|^dim[:digit:]|^title")]
    list_dim_column_label = list_dim_column[stringr::str_detect(list_dim_column, "_label_fr$|_label_en$")]
    list_dim_column_short = list_dim_column[!list_dim_column %in% list_dim_column_label]
    list_dim_ui = list()

    for(idim in 1:length(list_dim_column_short)){

      dim = list_dim_column_short[idim]
      dim_label_fr = list_dim_column_label[list_dim_column_label %in% paste0(dim, c("_label_fr"))]
      dim_label_en = list_dim_column_label[list_dim_column_label %in% paste0(dim, c("_label_en"))]
      
      if(length(dim_label_fr) > 0 & length(dim_label_en) > 0){
        
        df_dim = idbank_list_selected %>%
          select(!!dim, !!dim_label_fr, !!dim_label_en) %>%
          distinct()
        
      }else{
        df_dim = idbank_list_selected %>%
          select(!!dim) %>%
          distinct()
        
        df_dim[,paste0(dim, "_label_fr")] = ""
        df_dim[,paste0(dim, "_label_en")] = ""
      }
      
      df_dim[, "value_label"] = unlist(lapply(1:nrow(df_dim),
                                              function(i){
                                                if(is.na(df_dim[i,dim])){return(NA)}
                                                if(df_dim[i, paste0(dim, sprintf("_label_%s", lang_selected))] == ""){
                                                  return(df_dim[i,dim])
                                                }else{
                                                  return(paste(df_dim[i,dim], "-", df_dim[i,paste0(dim, sprintf("_label_%s", lang_selected))]))
                                                }
                                              }))
      
      if(lang_selected == "en"){
        dim_placeholder = sprintf("%s", dim)
      }else{
        dim_placeholder = sprintf("%s", dim)
      }
      
      value_dim = df_dim %>% pull(dim) 
      value_dim_label = df_dim %>% pull("value_label")
      
      value_dim = sort(value_dim[!is.na(value_dim)])
      value_dim_label = sort(value_dim_label[!is.na(value_dim_label)])
      
      list_dim_ui[[length(list_dim_ui)+1]] <-
        column(2,
               shinyWidgets::pickerInput(
                 inputId = paste0("dim", idim),
                 label = NULL,
                 choices = value_dim,
                 width = "100%",
                 multiple = TRUE,
                 options =
                   pickerOptions(
                     liveSearch = TRUE,
                     actionsBox = TRUE,
                     selectAllText = select_all_text,
                     deselectAllText = deselect_all_text,
                     dropdownAlignRight = "auto",
                     title = dim_placeholder
                   ),
                 choicesOpt = list(content = value_dim_label)
               )
        )
    }

    output$dims = renderUI({tagList(list_dim_ui)})

  }else{
    updateSelectizeInput(session, 'idbank_picker', choices = idbank_list_all, server = TRUE)
    output$dims = renderUI({NULL})
  }

  idbank_list_react(idbank_list_selected)
  idbank_list_view(idbank_list_selected)

  
  idbank_list_shown = idbank_list_selected %>%
    DT::datatable(filter = "none", selection = c("multiple"),
                  options = list(pageLength = 100, scrollX = TRUE,
                                 autoWidth = TRUE,
                                 columnDefs = list(list(width = '170px', targets = c(0)),
                                                   list(width = '130px', targets = c(1)),
                                                   list(width = '500px', targets = c(2)))), rownames = FALSE) %>%
    DT::formatStyle(0, lineHeight = '15px', target= 'row', textAlign = 'center')

  output$idbank_list <- DT::renderDT(idbank_list_shown)

  list_tab = list()

  list_tab[[length(list_tab)+1]] =
    tabPanel(title = get_label("idbank_list", lang = lang_selected),
             box(
               width = "100%",
               DT::dataTableOutput("idbank_list", width = "100%", height = "75vh")
             ))

  output$list_tab2 <- renderUI({
    do.call(tabsetPanel, c(list_tab, id = 'tabs2'))
  })

}, ignoreNULL = FALSE)

# observe({ 
#   Print(input$tabs_menu)
#   Print(input$sidebarmenu)
#   Print(input$sidebarItemExpanded)
#   })

observeEvent({
  input$dim1
  input$dim2
  input$dim3
  input$dim4
  input$dim5
  input$dim6
  input$dim7
  input$dim8
  input$dim9
  input$dim10
  input$dim11
  input$dim12
  input$dim13
  input$dim14
  input$dim15
  input$dim16
  input$idbank_picker
},{
  # Print(input$dim1)
  
  if(is.null(lang())){lang_selected = "fr"}else{lang_selected = lang()}

  
  idbank_list_selected = idbank_list_react()
  list_idbank_column = names(idbank_list_selected)
  list_dim_column = list_idbank_column[!stringr::str_detect(list_idbank_column, "^idbank|^cleFlow|^nomflow|^dim[:digit:]|^title")]
  list_dim_column_label = list_dim_column[stringr::str_detect(list_dim_column, "_label_fr$|_label_en$")]
  list_dim_column_short = list_dim_column[!list_dim_column %in% list_dim_column_label]
  
  any_dim_not_null = FALSE

  for(idim in 1:length(list_dim_column_short)){
    dim = list_dim_column_short[idim]

    if(length(dim) > 0) {
      if (!is.na(dim)) {
        any_dim_not_null = TRUE
        if (!is.null(input[[paste0('dim', idim)]])) {

          idbank_list_selected =
            idbank_list_selected %>%
            filter(!!sym(dim) %in% input[[paste0('dim', idim)]])
        }
      }
    }

  }

  if(!is.null(input$idbank_picker)){
    if(nrow(idbank_list_selected) > 0 ){

      any_dim_not_null = TRUE

      idbank_list_selected =
        idbank_list_selected %>%
        dplyr::filter(idbank %in% input$idbank_picker) %>%
        insee::clean_table()
    }
  }

  if(any_dim_not_null){

  idbank_list_view(idbank_list_selected)

  idbank_list_shown = idbank_list_selected %>%
    DT::datatable(filter = "none", selection = c("multiple"),
                  options = list(pageLength = 100, scrollX = TRUE,
                                 autoWidth = TRUE,
                                 columnDefs = list(list(width = '170px', targets = c(0)),
                                                   list(width = '130px', targets = c(1)),
                                                   list(width = '500px', targets = c(2)))), rownames = FALSE) %>%
    DT::formatStyle(0, lineHeight = '15px', target= 'row', textAlign = 'center')

  output$idbank_list <- DT::renderDT(idbank_list_shown)

    list_tab = list()

    list_tab[[length(list_tab)+1]] =
      tabPanel(title = get_label("idbank_list", lang = lang_selected),
               box(
                 width = "100%",
                 DT::dataTableOutput("idbank_list", width = "100%", height = "75vh")
               ))

    output$list_tab2 <- renderUI({
      do.call(tabsetPanel, c(list_tab, id = 'tabs2'))
    })
  }

}
, ignoreNULL = FALSE
)

observeEvent({
  input$deselect_idbank_in_list
},{
  idbank_list_proxy <- DT::dataTableProxy("idbank_list", session = session)
  DT::selectRows(idbank_list_proxy, NULL)
})


observeEvent({
  input$new_plot
  input$idbank_list_rows_selected
},{
  if(is.null(lang())){lang_selected = "fr"}else{lang_selected = lang()}

  row_selected = input$idbank_list_rows_selected
  
  # trigger_update
  if(TRUE){
    if(!is.null(row_selected)){
      
      idbank_selected =
        idbank_list_view() %>%
        slice(row_selected) %>%
        pull(idbank)
      
      data_raw = get_insee_idbank(idbank_selected)
      data = data_raw
      
      data = data %>%
        mutate(TITLE_EN = paste(TITLE_EN , "-", IDBANK)) %>%
        mutate(TITLE_FR = paste(TITLE_FR , "-", IDBANK))
      
      gg_name = paste0("plot_", gsub("-|:| |CET","", Sys.time()))
      gg_current_name(gg_name)
      
      output$growth_button <- renderUI({
        prettyRadioButtons(
          inputId = "growth_button",
          selected = input$growth_button,
          label = get_label("growth_button_title", lang = lang_selected),
          choiceNames = c(get_label("growth_button_raw", lang = lang_selected),
                          get_label("growth_button_yoy", lang = lang_selected),
                          get_label("growth_button_pop", lang = lang_selected)),
          choiceValues = c("raw", "yoy", "pop")
        )
      })
      
      if(!is.null(input$growth_button)){
        
        if(input$growth_button != "raw"){
          data = data %>%
            mutate(year = lubridate::year(DATE)) %>%
            arrange(DATE) %>%
            group_by(IDBANK, year) %>%
            mutate(freq = n()) %>%
            group_by(IDBANK) %>%
            mutate(freq = max(freq))
        }
        
        if(input$growth_button == "yoy"){
          
          data = data %>%
            mutate(OBS_VALUE_raw = OBS_VALUE) %>%
            group_by(IDBANK) %>%
            mutate(OBS_VALUE = 100 * (OBS_VALUE / abs(dplyr::lag(OBS_VALUE, min(freq))) - 1))
          
          if(lang_selected == "en"){
            data = data %>%
              mutate(TITLE_EN = paste(TITLE_EN , "-", get_label("growth_button_yoy", lang = lang_selected)))
          }else{
            data = data %>%
              mutate(TITLE_FR = paste(TITLE_FR , "-", get_label("growth_button_yoy", lang = lang_selected)))
          }
        }
        
        if(input$growth_button == "pop"){
          data = data %>%
            mutate(OBS_VALUE_raw = OBS_VALUE) %>%
            group_by(IDBANK) %>%
            mutate(OBS_VALUE = 100 * (OBS_VALUE / abs(dplyr::lag(OBS_VALUE, 1)) - 1))
          
          data = data %>%
            mutate(
              add_title_period = case_when(
                freq == 12 ~ get_label("growth_button_mom", lang = lang_selected),
                freq == 4 ~ get_label("growth_button_qoq", lang = lang_selected),
                freq == 6 ~ get_label("growth_button_bob", lang = lang_selected),
                freq == 2 ~ get_label("growth_button_sos", lang = lang_selected),
                freq == 1 ~ get_label("growth_button_yoy", lang = lang_selected)
              )
            )
          
          if(lang_selected == "en"){
            data = data %>%
              mutate(TITLE_EN = paste(TITLE_EN , "-", add_title_period)) %>%
              select(-add_title_period)
          }else{
            data = data %>%
              mutate(TITLE_FR = paste(TITLE_FR , "-", add_title_period)) %>%
              select(-add_title_period)
          }
        }
        
        data = data %>% ungroup() %>% arrange(desc(DATE))
      }
      
      data_plot(data)
      
      if(is.null(input$slider_period)){
        min_slider_period =  min(data_raw$DATE)
        max_slider_period =  max(data_raw$DATE)
      }else{
        min_slider_period = input$slider_period[1]
        max_slider_period = input$slider_period[2]
      }
      
      output$slider_period <- renderUI({
        sliderInput("slider_period",
                    label = get_label("slider_period", lang = lang_selected),
                    min = min(data_raw$DATE),  max = max(data_raw$DATE),
                    value = c(min_slider_period, max_slider_period))
      })
  
      data = data %>%
        filter(DATE >= min_slider_period & DATE <= max_slider_period)
      
      data$TITLE_EN = unlist(lapply(strwrap(data$TITLE_EN,
                                            width = 80, simplify=FALSE),
                                    paste, collapse="\n"))
      
      data$TITLE_FR = unlist(lapply(strwrap(data$TITLE_FR,
                                            width = 80, simplify=FALSE),
                                    paste, collapse="\n"))
      
      gg =
        ggplot(data, aes(x = DATE, y = OBS_VALUE))
      
      
      if(!get_one_plot()){
        if(lang_selected == "en"){
          gg = gg +
            facet_wrap(~TITLE_EN, scales = "free", dir = "v")
        }else{
          gg = gg +
            facet_wrap(~TITLE_FR, scales = "free", dir = "v")
        }
        gg = gg +
          geom_line() +
          geom_point(size = 1.5)
      }else{
        if(lang_selected == "en"){
          gg = gg +
            geom_line(aes(colour = TITLE_EN)) +
            geom_point(aes(colour = TITLE_EN), size = 1.5)
          
        }else{
          gg = gg +
            geom_line(aes(colour = TITLE_FR)) +
            geom_point(aes(colour = TITLE_FR), size = 1.5)
        }
        gg = gg + guides(colour = guide_legend(ncol = 1))
      }
      
      gg = gg %>% add_style(lang = lang_selected)
      
      # gg_current(gg)
      list_plot_selected[[gg_name]] = gg
      
      output[[gg_name]] <- renderPlot({gg})
      
      if(!get_interactive_plot()){
        
        tab = tabPanel(title = get_label("plot_catalogue", lang = lang_selected),
                       box(
                         width = "100%",
                         plotOutput(gg_name, width = "100%", height = "80vh")
                       ))
        
      }else{
        
        output[["plotly_requested"]] <- plotly::renderPlotly({gg_plotly(data, lang = lang_selected)})
        
        tab =   tabPanel(title = get_label("plot_catalogue", lang = lang_selected),
                         box(
                           width = "100%",
                           plotlyOutput("plotly_requested", width = "100%", height = "80vh")
                         ))
        
      }
      
      removeTab(inputId = "tabs2",
                target = get_label("plot_catalogue", lang = lang_selected))
      
      insertTab(inputId = "tabs2",
                tab = tab,
                position = "after", 
                target = get_label("idbank_list", lang = lang_selected),
                select = TRUE)
      
      if(lang_selected == "en"){
        data_selected = data %>% select(-TITLE_FR)
      }else{
        data_selected = data %>% select(-TITLE_EN)
      }
      
      data_shown =
        data_selected %>%
        arrange(desc(DATE)) %>%
        DT::datatable( filter = "none",
                       options = list(pageLength = 100, scrollX = TRUE,
                                      autoWidth = TRUE,
                                      columnDefs = list(list(width = '400px', targets = c(8)))
                       ),
                       rownames = FALSE) %>%
        DT::formatStyle(0, lineHeight = '15px', target = 'row', textAlign = 'center')
      
      output$data_table <- DT::renderDT(data_shown)
      
      
      tab_data =     tabPanel(title = get_label("data_table", lang = lang_selected),
                              box(
                                width = "100%",
                                DT::dataTableOutput("data_table", width = "100%", height = "75vh")
                              ))
      
      removeTab(inputId = "tabs2",
                target = get_label("data_table", lang = lang_selected))
      
      insertTab(inputId = "tabs2",
                tab = tab_data,
                position = "after", 
                target = get_label("plot_catalogue", lang = lang_selected),
                select = FALSE)
      
    }
  }
})

  observeEvent({
    input$catalogue_cell_clicked
  },{

    row_selected = input$catalogue_cell_clicked$row[1]

    if(!is.null(row_selected)){

      gg_selected =
        plot_table_react() %>%
        slice(row_selected) %>%
        pull(id)

      if(is.null(lang())){lang_selected = "fr"}else{lang_selected = lang()}

      if(lang_selected == "en"){

        plot_table_shown =
          plot_table %>%
          dplyr::select(id, title_en) %>%
          dplyr::rename(Title = title_en, Identifier = id)
      }else{

        plot_table_shown =
          plot_table %>%
          dplyr::select(id, title_fr) %>%
          dplyr::rename(Titre = title_fr, Identifiant = id)
      }

      plot_table_shown2 = 
        plot_table_shown %>% 
        DT::datatable(
          filter = "none",
          options = list(pageLength = 100),
          rownames = FALSE
        ) %>%
        DT::formatStyle(0, lineHeight = '15px', target = 'row')
      
      output$catalogue <- DT::renderDT(plot_table_shown2)

      gg_statement = sprintf("%s(lang='%s')", gg_selected, lang_selected )

      gg = eval(parse(text = gg_statement))

      # gg_current(gg)
      gg_current_name(gg_selected)
      list_plot_selected[[gg_selected]] = gg

      output[[paste0(gg_selected, "_plot")]] <- renderPlot({gg})

      data_plot(gg[["data"]])

      list_tab = list()

      list_tab[[length(list_tab)+1]] =
        tabPanel(title = "Catalogue",
                 box(
                   width = "100%",
                   DT::dataTableOutput("catalogue", width = "100%", height = "75vh")
                 ))
      
      list_tab[[length(list_tab)+1]] =
        tabPanel(title = get_label("plot_catalogue", lang = lang_selected),
                 box(
                   width = "100%",
                   plotOutput(paste0(gg_selected, "_plot"), width = "100%", height = "80vh")
                 ))
      
      list_tab[[length(list_tab)+1]] =
        tabPanel(title = "Code",
                 box(
                   width = "100%",
                   renderPrint({print(gg_selected);get(gg_selected)})
                 ),
                 box(
                   width = "100%",
                   renderPrint({print("add_style");add_style})
                 )
                 )

      output$list_tab <- renderUI({
        do.call(tabsetPanel, c(list_tab, id = 'tabs',
                               selected = get_label("plot_catalogue", lang = lang_selected)))
      })

    }

  })

  #
  # csv download
  #
  observe({

    input$downloadData

    data = data_plot()

    if(!is.null(data)){
      if(any(class(data) == "data.frame")){

        output$downloadData <- downloadHandler(
          filename = function() {
            paste("data_", gsub("-|:| |CET","", Sys.time()), ".csv", sep="")
          },
          content = function(file) {
            utils::write.csv(data, file,  row.names = FALSE)
          }
        )

      }
    }

  })


  observeEvent({
    gg_current_name()
    },{
    updateSelectizeInput(session, "slides",
                         choices = c(gg_current_name(), input$slides), selected = input$slides)
  })

  #
  # DOWNLOAD SLIDES
  #
  observe({
    list_plot_slides = lapply(input$slides, function(x) list_plot_selected[[x]])

    output$downloadSlides <- downloadHandler(
      filename = function() {
        file.path(Sys.getenv("USERPROFILE"), "Desktop", 
                  paste("graffiti_slides_", gsub("-|:| |CET","", Sys.time()), ".pdf", sep=""))
      },
      content = function(file) {
        rmarkdown::render(input = slides_rmd_file,
                          output_file = file,
                          params = list(list_graph = list_plot_slides))
      }
    )

  })



}
)
