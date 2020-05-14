
ui <- dashboardPage(

                   
  dashboardHeader(title = ""),
  
  dashboardSidebar(


    
    
    fileInput("categorisation_file",
          "Upload your own file:",
          multiple = FALSE,
          accept = c(".xlsx")) ,
    
    
    
    ###################################################################
    downloadButton('downloadData','Download') , 
    
  
    
    ###################################################################
    
    
    sliderInput("forecast_months",
            "Forecast months ahead:", 
            12, 
            min = 1, 
            max = 24) ,
    

radioButtons("display_options_id",
             "Chart display:", 
             choices = display_options, 
             selected = display_options[1]) ,



radioButtons("level_options_id",
             "Breakdown by:", 
             choices = level_options, 
             selected = level_options[1])
    

  , uiOutput("level_2_filter")
  
  
  , uiOutput("level_1_filter")



    ),
  
  
###################################################################



dashboardBody(
  
  fluidRow(
    
    DTOutput("x1")
    

  ) , 

  fluidRow(
    
    plotlyOutput("plot1")
    

  )
)


)


