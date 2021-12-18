observe({
  #Set disable as default to imported files options dropdown
  disable("imported_DD")
  
  #Set disable as default to delimiter picker input
  #by giving an id to the button and disable it
  runjs('var delimeterbttn = document.querySelector("#delimiter ~ button");
          delimeterbttn.setAttribute("id","delimiterbttn")')
  disable("delimiterbttn")
  
  #Set disable as default to dm picker input by giving
  #an id to the button and disable it
  runjs('var dmbttn = document.querySelector("#dm ~ button");
          dmbttn.setAttribute("id","dmbttn")')
  disable("dmbttn")
  
  #Set hide as default to "tableandgraphs" tabset panel
  hide("tableandgraphs")
  
  #Set disable as default select variables dropdown
  disable('selectvariables')
  
  #Set hide as default to tabpanel "Graphs" of  "tableandgraps" tabset panel
  hideTab("tableandgraphs", "Graphs")
  
  #Set hide as default to tabpanel "Graphs" of  "tableandgraps" tabset panel
  hideTab("tableandgraphs", "EDA")
  
  #Set default behavior of selectamounttouse dropdown
  click("selectamounttouse")
  click("selectamounttouse")
  disable("selectamounttouse")
  
  #Set hidden as default for the select test start train date table and it's button
  hide("containerofstd")
  hide("eliminatetrainsd")
  
})

#To enable or no the Imported file options 
#if a file with the proper extension is imported
observeEvent(input$file, {
  
  #If a file with the proper extension is imported show the imported file options
  if(is.element(file_ext(input$file$datapath), readall)){
    enable("imported_DD")
    show("tableandgraphs")
    hide('fileupload')
    enable('selectvariables')
    show('files')
    show("summaryandgraph")
    
    #If a file that need delimiter and decimal mark parameters is imported show them
    if(is.element(file_ext(input$file$datapath), readdelim)){
      enable("delimiterbttn")
      enable("dmbttn")
    }
    
    #If a file that does not need delimiter and decimal mark parameters is imported
    #hide them
    if(is.element(file_ext(input$file$datapath), readexcel)){
      disable("delimiterbttn")
      disable("dmbttn")}
    
    #If a file with another extension is imported keep imported file options hidden or
    #hide it
  }else{
    disable("imported_DD")
    hide("tableandgraphs")
    show('fileupload')
    disable('selectvariables')
    hide('files')
    hide("summaryandgraph")
  }
})

#To upload file
output$fileupload <- renderUI({
  if(is.null(input$file))
    tags$h4(tags$strong("Upload file"), align = "center", style = "color:black")
  else if(!is.element(file_ext(input$file$datapath), readall))
    tags$h4(
      tags$strong("The upload file is not txt, csv, tsv, fwf, xlsx or xls"),
      align = "center",
      style = "color:black")
  
})

#Imported file name
output$filename <- renderUI({
  tags$h5(tags$strong(input$file$name), align = "center")
})

#Imported file table
#Reactive value to store the imported file
database <- reactiveValues()
database$df <- data.frame()

#Giving value to reactive database$df in dependency of the imported file
#and the imported file options selected by the user
observeEvent(c(input$file,input$header,input$delimiter,input$dm),{
  if(is.null(input$file$datapath)){}
  else{
    if(is.element(file_ext(input$file$datapath), readdelim)){
      database$df <- read_delim(input$file$datapath,
                                delim = input$delimiter,
                                col_names = input$header,
                                locale = locale(decimal_mark = input$dm))
    }else{
      if(is.element(file_ext(input$file$datapath), readexcel)){
        database$df <- read_excel(
          input$file$datapath,
          col_names = input$header)
      }
    }
    
    if(any(is.na(database$df))){
      database$df <- na.omit(database$df)
      shinyalert("Warning",
                 "Uploaded database has NaN values, rows with NaN values has been removed",
                 type = 'warning')
    }
      
  }
})

#Rendering the imported database stored in reactive value
output$files <- renderDataTable(
  database$df,
  options = list(searching = F,
                 scrollX = T,
                 compact = F,
                 lengthChange = F,
                 pageLength = 7,
                 ordering = F),
  class = "nowrap hover order-column")

#Update pickerInput datevariable
observeEvent(database$df,{
  updatePickerInput(session = session,
                    inputId = "datevariable",
                    choices = colnames(database$df))
  
  
})

#Creating input-output grid
#Reactive value to store the grid
database$grid <- data.frame()
#Setin grid dataframe
observeEvent(c(database$df,input$datevariable),{
  if(is.null(input$datevariable)||input$datevariable==""){
    data <- database$df
  }else{
    data <- database$df %>% select(-input$datevariable)
  }
  colamount <- dim(data)[2]
  variables <- colnames(data)
  database$grid <- data.frame(Inputs = rep(F,colamount),
                              Outputs = rep(F,colamount),
                              Variables = variables)
  
})

#Styling grid
output$io_gridtable <- renderRHandsontable({
  rhandsontable(database$grid,
                disableVisualSelection = T,
                height = 175) %>%
    hot_col("Variables", readOnly = T)
})

#EDA plotting
#To store EDA
database$EDA <- data.frame()
#To show EDA
database$showEDA <- 0
#EDA of selcted variables in gridtable
observeEvent(input$io_gridtable,{
  if(any(hot_to_r(input$io_gridtable) == 1)){
    #To show and select EDA only if it is first time an input or output
    #variable is selected
    if(database$showEDA < 2){
      database$showEDA <- database$showEDA + 1
    }
    data <- hot_to_r(input$io_gridtable)
    inp <- data %>% filter(Inputs == 1) %>% select(Variables)
    out <- data %>% filter(Outputs == 1) %>% select(Variables)
    variables <- merge(inp,out,all = T)[[1]]
    data <- database$df[variables]
    if(identical(data,database$previousEDA)){
      
    }else{
      database$EDA <- data
      showTab("tableandgraphs", "EDA")
      enable("selectamounttouse")
      
      if(database$showEDA == 1){
        updateTabsetPanel(
          session,
          "tableandgraphs",
          "EDA")
      }
    }
  }
})

#EDA render plot when clicking in grid table
output$eda <- renderPlot({
  data <- future_promise({database$EDA})
  data %...>%{
    data <- .
    ggpairs(data,
            title = 'Exploratory Data Analysis',
            lower = list(continuous = wrap("points",colour = "blue")),
            diag = list(continuous = wrap('densityDiag', color = "black", fill = "blue", alpha = 0.5)),
            upper = list(continuous = wrap('cor', size = 6)))+
      theme(plot.title = element_text(hjust = 0.5),
            text = element_text(size=15))
  }
})

#EDA render summary table when clicking in grid table
output$summary <- renderDT({
  data <- future_promise({database$EDA})
  data %...>%{
    data <- .
    stat.desc(data)
  }
},
options = list(
  dom = "t"
),
class = "nowrap hover order-column")

#Select dates-periods
#To store the X axis(date-period)
database$Xdata <- NULL
database$previousXdata <- NULL
#When selectamounttouse dropdown is clicked X axis is stored either from a previously
#selected variable or from a consecutive vector
#Start test variable is 2nd on the stored x axis, start test variable will be limited
#from 2nd to (last - 1) to ensure that at least one start train and end test are
#selected
observeEvent(input$selectamounttouse,{
  if(is.null(input$datevariable)||
     !identical(database$df[[input$datevariable]], unique(database$df[[input$datevariable]]))){
    database$Xdata <- 1:dim(database$EDA)[1]
    
  }else{
    database$Xdata <- as.character(database$df[[input$datevariable]])
  }
  if(length(database$Xdata) == dim(database$EDA)[1]){
    if(is.null(database$previousXdata) ||
       database$previousXdata != database$Xdata){
      database$previousXdata <- database$Xdata
      updatePickerInput(session,
                        'selectteststart',
                        choices = database$Xdata[2:(length(database$Xdata)-1)])
    }else{
      
    }
  }
  
  
})

#When changing the selected start test date options of select end test date are
#updated. test end date will be limited from (start test date + 1) to last period.
observeEvent(input$selectteststart,{
  Xdata <- database$Xdata
  start <- input$selectteststart
  end <- input$selecttestend
  choices2 <- Xdata[(which(Xdata == start)+1):length(Xdata)]
  
  if(is.null(end) || !any(choices2 == end)){
    selected2 <- NULL
  }else{
    selected2 <- end
  }
  
  updatePickerInput(session,
                    'selecttestend',
                    choices = choices2,
                    selected = selected2)
})

#When changing the selected start test date options of select start train date are
#updated. start train date will be limited from 1st period to (start test date - 1).
database$starttrainlevels <- NULL
observeEvent(input$selectteststart,{
  Xdata <- database$Xdata
  end <- input$selectteststart
  start <- input$selecttrainstart
  choices <- Xdata[1:(which(Xdata == end)-1)]
  database$starttrainlevels <- choices
  if(is.null(start) ||
     !is.element(start,Xdata) ||
     which(Xdata==start) >= which(Xdata==end)){
    selected <- NULL
  }else{
    selected <- start
  }
  
  updatePickerInput(session,
                    'selecttrainstart',
                    choices = choices,
                    selected = selected)
})

#To save selected train periods
database$selectedtrains <- data.frame()
#To show or no the Graphs tab
database$showGraphs <- 0
#When ok button to add a train period is clicked.
#   * Will be checked if there are at least 1 input and 1 output variable and if
#     if the test period has been defined
#   * The table containing the train dates selected will be updated if require,
#     either if a new entry has been done or if any entry most be remove because exceed
#     the actual start test period date.
#   * And Graphs tab,table of trains date and the eliminate button will be shown the
#     first time the ok button is clicked successfully 
observeEvent(input$adtraintotest,{
  if(is.null(input$selectteststart) ||
     is.null(input$selecttestend) ||
     is.null(input$selecttrainstart) ||
     !any(hot_to_r(input$io_gridtable)$Inputs == 1) ||
     !any(hot_to_r(input$io_gridtable)$Outputs == 1)
  ){
    
    shinyalert("Error","Make sure you have selected Input and Output variables,
                 start and end dates to the test set and at least an start date for
                 training set",type = 'error')
    
  }else{
    stns <- input$selecttrainstart
    if(dim(database$selectedtrains)[1] == 0){
      database$selectedtrains <- data.frame(`Train start dates` = stns)
      show("containerofstd")
      show("eliminatetrainsd")
      showTab("tableandgraphs", "Graphs")
      
      if(database$showGraphs < 2){
        database$showGraphs <- database$showGraphs + 1
      }
      
      if(database$showGraphs == 1){
        updateTabsetPanel(
          session,
          "tableandgraphs",
          "Graphs")
      }
    }else{
      if(!is.element(stns,database$selectedtrains[,1])){
        database$selectedtrains <- rbind(database$selectedtrains,stns)
        database$selectedtrains <- database$selectedtrains %>%
          arrange(factor(Train.start.dates, levels = database$starttrainlevels))
      }
      
    }
    
    indxofstn <- which(is.element(database$Xdata,
                                  database$selectedtrains$Train.start.dates))
    indxofstt <- which(database$Xdata == input$selectteststart)
    if(any(indxofstn > indxofstt)){
      whichisbig <- which(indxofstn > indxofstt)
      database$selectedtrains <- database$selectedtrains %>%
        slice(-whichisbig)
    }
    
  }
})

#To render the table of the selected start train dates
output$traindatestable <- renderDT({
  datatable(database$selectedtrains,
            options = list(
              dom = "t",
              pageLength = dim(database$selectedtrains)[1]
            ))
})

#To eliminate options of select train start table
observeEvent(input$eliminatetrainsd,{
  rwstr <- input$traindatestable_rows_selected
  database$selectedtrains <- database$selectedtrains %>%
    slice(-rwstr)
})

#To create output of the start train and dates sets
#It will be checked that there are at least 1 input and 1 output variable selected and
#that there is at least 1 start train date selected.
#The a grid of plot will be create, containing a plot for each start train date added.
#The plot will consist of all the variables and it train and test periods.
#The grid plot will be updated whit any change in the selected start train periods and
#any change regarding the selected variables.
observeEvent(c(input$adtraintotest,input$io_gridtable, input$eliminatetrainsd),{
  
  if(dim(database$selectedtrains)[1]>0 &&
     any(hot_to_r(input$io_gridtable)$Inputs == 1) &&
     any(hot_to_r(input$io_gridtable)$Outputs == 1)){
    
    for (i1 in 1:dim(database$selectedtrains)[1]) {
      outputname <- paste0('plotof',i1)
      if(!(i1%%2) == 0){
        element <- tagList(
          div(
            plotlyOutput(outputname),
            style = "float:left; width:48%;"
          )
        )
        
      }else{
        element <- tagList(
          div(
            plotlyOutput(outputname),
            style = "float:right; width:48%;"
          )
        )
      }
      
      if(i1 == 1){
        
        database$SVplots <- element
        
      }else{
        
        database$SVplots <- tagList(database$SVplots,element)
      }
      
      output$plotselectedvariables <- renderUI({
        database$SVplots
      })
      
      local({
        
        p <- plot_ly(type = "scatter", mode = "lines") %>%
          config(displayModeBar = F)
        
        for (i2 in 1:dim(database$EDA)[2]) {
          stn <- which(database$Xdata == database$selectedtrains[i1,])
          stt <- which(database$Xdata == input$selectteststart)
          x1 <- database$Xdata[stn:stt]
          y1 <- database$EDA[stn:stt,i2][[1]]
          p <- p %>% add_trace(
            x = x1,
            y = y1,
            mode = "lines",
            legendgroup = colnames(database$EDA)[i2],
            legendgrouptitle = list(text = colnames(database$EDA)[i2]),
            name = 'train')
          
          linegtcolr <- i2 * 2
          
          ptogtcolr <- plotly_build(p)
          nxtlinecolor <- ptogtcolr$x$data[[linegtcolr]]$line$color[1]
          ett <- which(database$Xdata == input$selecttestend)
          x2 <- database$Xdata[stt:ett]
          y2 <- database$EDA[stt:ett,i2][[1]]
          
          p <- p %>% add_trace(
            x = x2,
            y = y2,
            mode = "lines",
            legendgroup = colnames(database$EDA)[i2],
            name = 'test',
            line = list(dash = "dash",
                        color = nxtlinecolor))
          
        }
        
        output[[outputname]] <- renderPlotly({
          
          p 
          
        })
        
      })
    }
  }
})