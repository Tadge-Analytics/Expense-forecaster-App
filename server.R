
server <- function(input, output) {
  
  
  
  
  
  x <- reactiveValues(df = NULL)
  x$df <- blank_table
  
  
  output$x1 <- renderDataTable(x$df, 
                               selection = 'none', 
                               editable = TRUE, 
                               options = list(
                                 searching = TRUE,
                                 pageLength = -1,
                                 scrollY = "275px",
                                 lengthMenu = lengthMenu_options))
  
  
  observeEvent(input$x1_cell_edit, {
    
    info <- input$x1_cell_edit
    
    i <- info$row
    j <- info$col
    v <- info$value
    
    x$df[i, j] <- input$x1_cell_edit$value
    
  })
  
  
  
  observeEvent(input$categorisation_file, {
    
    x$df <- input$categorisation_file$datapath %>%
      process_the_rates_file()
  })
  
  
  
  
  ###################################################################
  
  
  
  
  days_to_add <- reactive(
    input$forecast_months * 31
  )
  
  
  
  data_for_plot <- reactive(
    
    x$df %>% 
      prep_from_rates_and_dates(days_to_add = days_to_add()) %>% 
      filter(Amount != 0)
    
  )
  
  
  
  output$plot1 <- renderPlotly({
    
    data_for_plot() %>% 
      pre_plotting_data_prep(days_to_add = days_to_add(), chart_setting = input$display_options_id) %>% 
      
      
      {if (!is.null(input$Level2_options)) {filter(., `Category Level2` %in% input$Level2_options)} else {.}} %>%
      {if (!is.null(input$Level1_options)) {filter(., `Category Level1` %in% input$Level1_options)} else {.}} %>%
      
      
      plot_the_data(view_level = input$level_options_id, chart_setting = input$display_options_id)
    
  }) 
  
  
  
  ###################################################################
  
  Level2_options <- reactive(
    
    data_for_plot() %>%
      distinct(`Category Level2`) %>%
      pull()
  )
  
  
  
  output$level_2_filter <- renderUI(
    pickerInput(
      inputId = "Level2_options",
      label = "Level 2 filter",
      choices = Level2_options(),
      options = list(
        `actions-box` = TRUE
      ),
      multiple = TRUE
    )
  )
  
  
  
  
  
  
  Level1_options <- reactive(
    data_for_plot() %>%
      {if (!is.null(input$Level2_options)) {filter(., `Category Level2` %in% input$Level2_options)} else {.}} %>%
      distinct(`Category Level1`) %>%
      pull()
  )
  
  
  
  output$level_1_filter <- renderUI(
    pickerInput(
      inputId = "Level1_options",
      label = "Level 1 filter",
      choices = Level1_options(),
      options = list(
        `actions-box` = TRUE
      ),
      multiple = TRUE
    )
  )
  
  
  
  
  
  
  
  ###################################################################
  
  ready_for_download <- reactive(
    x$df %>% 
      correct_col_types()
  )
  
  
  forecasted_instances <- reactive(
    x$df %>%
      prep_from_rates_and_dates(days_to_add = days_to_add())
  )
  
  
 

  
  
  
  
  
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      "Categorisations done.xlsx"
    },
    content = function(file) {
      
      my_workbook <- createWorkbook()
      
      addWorksheet(
        wb = my_workbook,
        sheetName = "forecast rates"
      )
      
      writeDataTable(
        my_workbook,
        sheet = "forecast rates",
        ready_for_download(),
        tableStyle = "TableStyleMedium2", withFilter = T
      )
      
      addWorksheet(
        wb = my_workbook,
        sheetName = "forecast values"
      )
      
      
      writeDataTable(
        my_workbook,
        sheet = "forecast values",
        forecasted_instances(),
        tableStyle = "TableStyleMedium2", withFilter = T
      )

      
      saveWorkbook(my_workbook, file)
    }
  )
  
 
  
}

