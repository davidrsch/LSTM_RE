#SERVER CODE----
#SELECTING FEATURES----
#PANEL----
#1-Default style of UI ----
observe({
  #establish selecting features tab as disabled by default
  disable(selector = '#Main_tabsetpanel a[data-value = "Selecting Features"]')
  #Hide div for ts containing values equal or smaller than 0 as default
  hide("valuesequalorless0")
  #Hide div for stationary ts as default
  hide("Stationaryts")
  #Hide repeat experimentation button as default
  hide("repeatexperimentation")
  
  
})
#2-Enabling UI tab----
#To enable selecting features tab if a proper start training period is added
observeEvent(input$adtraintotest,{
  if(is.null(input$selectteststart) ||
     is.null(input$selecttestend) ||
     is.null(input$selecttrainstart) ||
     !any(hot_to_r(input$io_gridtable)$Inputs == 1) ||
     !any(hot_to_r(input$io_gridtable)$Outputs == 1)
  ){}else{
    enable(selector = '#Main_tabsetpanel a[data-value = "Selecting Features"]')
  }
  
})


#3-To store selecting features reactive values----
sf <- reactiveValues()
#4-Transformations dropdown----
##4.1-Selecting ts and scale explanation tab----
observeEvent(input$ts_and_scales_dd,{
  updateTabsetPanel(session = session,
                    'Proposal_tabsetPanel',
                    selected = "TS_and_scales_opt_tabPanel")
  
})
##4.2-Reactive value to store transformations----
sf$transf <- c("Original",
               "First transformation",
               "Second transformation")
##4.3-Enabling or no transf----
#To update if necessary the picker transformations. Picker transformations will
#be updated if.
#   Any ts has values equal or smaller than 0
#   Any ts is stationary
observeEvent(input$Main_tabsetpanel,{
  if(input$Main_tabsetpanel == "Selecting Features"){
    
    showvalues <- "hide"
    showstationary <- "hide"
    
    #Update select time series to previous state
    updatePickerInput(session = session,
                      inputId = "selectimeseries",
                      selected = sf$transf,
                      choices = c("Original",
                                  "First transformation",
                                  "Second transformation"),
                      choicesOpt = list(
                        disabled = c(F,F,F)
                      ))
    
    #To find out if there is any ts with values equal or smaller than 0 or if any
    #ts is stationary
    for (i2 in 1:dim(database$EDA)[2]) {
      stn <- which(database$Xdata == database$selectedtrains[1,])
      ett <- which(database$Xdata == input$selecttestend)
      y1 <- database$EDA[stn:ett,i2][[1]]
      
      if(any(y1 <=0 )){
        showvalues <- "show"
      }else{}
      
      ADF <- adf.test(y1, alternative = "stationary")
      PP <- tryCatch(pp.test(y1, alternative = "stationary"),
                     error = function(e)e)
      if(
        is.na(ADF$p.value) && identical(class(PP), errorclasses) ||
        is.na(ADF$p.value) && PP$p.value <= 0.05||
        ADF$p.value <= 0.05 && identical(class(PP), errorclasses)||
        ADF$p.value <= 0.05 && PP$p.value <= 0.05){
        showstationary <- "show"
      }
      
    }
    
    #To show or hide div of alert if there is or there isn't a ts with values <=0
    if(showvalues == "show"){
      show("valuesequalorless0")
      updatePickerInput(session = session,
                        inputId = "selectimeseries",
                        selected = "Original",
                        choices = c("Original",
                                    "First transformation",
                                    "Second transformation"),
                        choicesOpt = list(
                          disabled = c(F,F,T)
                        ))
    }else{
      hide("valuesequalorless0")
    }
    
    #To show or hide af alert if there is or there isn't a stationary ts
    if(showstationary == "show"){
      show("Stationaryts")
      updatePickerInput(session = session,
                        inputId = "selectimeseries",
                        selected = "Original",
                        choices = c("Original",
                                    "First transformation",
                                    "Second transformation"),
                        choicesOpt = list(
                          disabled = c(F,T,T)
                        ))
    }else{
      hide("Stationaryts")
    }
  }
})

##4.4-Storing transformation in RV----
observeEvent(input$selectimeseries,{
  sf$transf <- input$selectimeseries
})

#5-Training vectors dopdown----
##5.1-Selecting training vectors explanation tab----
observeEvent(input$tv_opt_dd,{
  updateTabsetPanel(session,
                    'Proposal_tabsetPanel',
                    selected = "Training_vectors_tabPanel")
})

##5.2-Reactive values of training vectors----
#To store input amounts of training vectors
sf$inputamnts <- NULL
#To store selected inputs amounts of training vectors
sf$stdinputamnts <- NULL

##5.3-Accepting an input amount----
observeEvent(input$acceptinputoptionbutton,{
  
  #To trigger error alert if introduced input amount not fulfill conditions
  #   integer and bigger than 0
  if(is.na(input$addINoption) ||
     input$addINoption < 1 ||
     !is.integer(input$addINoption)){
    shinyalert("Error",
               "Wrong input format. Input most be an integer number bigger than 0",
               type = 'error')
  }else{
    if(is.null(sf$inputamnts)){
      sf$inputamnts <- input$addINoption
      sf$stdinputamnts <- input$addINoption
    }else{
      if(!is.element(input$addINoption,sf$inputamnts)){
        sf$inputamnts <- c(sf$inputamnts, input$addINoption)
        sf$stdinputamnts <- c(sf$stdinputamnts, input$addINoption)
      }
    }
    
    #Update select input amounts of training vectors
    updatePickerInput(session,
                      'selectinputoptions',
                      selected = sf$stdinputamnts,
                      choices = sf$inputamnts)
  }
  
  
})

##5.4-Updating selected inputs values when selecting or deselecting----
observeEvent(input$selectinputoptions,{
  sf$stdinputamnts <- input$selectinputoptions
})

#6-Models dropdown----
##6.1-Selecting models explanation tab----
observeEvent(input$models_opt_dd,{
  updateTabsetPanel(session,
                    'Proposal_tabsetPanel',
                    selected = 'Moedels_tabPanel')
})

##6.2-Reactive values to LSTM----
#To store LSTM layers amounts
sf$LSTMamnts <- NULL
#To store selected LSTM layers amounts
sf$stdLSTMamnts <- NULL

##6.3-Accepting a LSTM layer amount----
observeEvent(input$acceptLSTMamountbutton,{
  
  #To trigger error alert if introduced LSTM layer amount not fulfill conditions
  #   integer and bigger than 0
  if(is.na(input$addLSTMamount) ||
     input$addLSTMamount < 1 ||
     !is.integer(input$addLSTMamount)){
    shinyalert("Error",
               "Wrong amount of LSTM layers format, most be an integer number bigger than 0",
               type = 'error')
  }else{
    if(is.null(sf$LSTMamnts)){
      sf$LSTMamnts <- input$addLSTMamount
      sf$stdLSTMamnts <- input$addLSTMamount
    }else{
      if(!is.element(input$addLSTMamount,sf$LSTMamnts)){
        sf$LSTMamnts <- c(sf$LSTMamnts, input$addLSTMamount)
        sf$LSTMamnts <- sort(sf$LSTMamnts)
        sf$stdLSTMamnts <- c(sf$stdLSTMamnts, input$addLSTMamount)
      }
    }
    
    #Update select LSTM layer amounts
    updatePickerInput(session,
                      'selectLSTMsoptions',
                      selected = sf$stdLSTMamnts,
                      choices = sf$LSTMamnts)
  }
  
  
})

##6.4-Updating selected LSTM layers when selecting or deselecting----
observeEvent(input$selectLSTMsoptions,{
  sf$stdLSTMamnts <- input$selectLSTMsoptions
})

##6.5-Reactive values to Neurons amount----
#To store neurons amounts
sf$neuronsamnts <- NULL
#To store selected neurons amounts
sf$stdneuronsamnts <- NULL

##6.6-Accepting a neuron amount----
observeEvent(input$acceptneuronamountbutton,{
  #To trigger error alert if introduced neuron amount not fulfill conditions
  #   integer and bigger than 0
  if(is.na(input$addneuronsamount) ||
     input$addneuronsamount < 1 ||
     !is.integer(input$addneuronsamount)){
    shinyalert("Error",
               "Wrong amount of neurons format, most be an integer number bigger than 0",
               type = 'error')
  }else{
    if(is.null(sf$neuronsamnts)){
      sf$neuronsamnts <- input$addneuronsamount
      sf$stdneuronsamnts <- input$addneuronsamount
    }else{
      if(!is.element(input$addneuronsamount,sf$neuronsamnts)){
        sf$neuronsamnts <- c(sf$neuronsamnts, input$addneuronsamount)
        sf$neuronsamnts <- sort(sf$neuronsamnts)
        sf$stdneuronsamnts <- c(sf$stdneuronsamnts, input$addneuronsamount)
      }
    }
    
    #Update select neuron amount
    updatePickerInput(session,
                      'selectneuronsoptions',
                      selected = sf$stdneuronsamnts,
                      choices = sf$neuronsamnts)
  }
  
  
})

##6.7-Updating selected neurons amounts when selecting or deselecting----
observeEvent(input$selectneuronsoptions,{
  sf$stdneuronsamnts <- input$selectneuronsoptions
})

#7-Training dropdown----
##7.1-Selecting training explanation tab----
observeEvent(input$training_opt_dd,{
  updateTabsetPanel(session,
                    'Proposal_tabsetPanel',
                    selected = "Training_options_tabPanel")
})

##7.2-Enabling seed based on use seed configuration----
observeEvent(input$setseed,{
  if(input$setseed == T){
    enable("seed")
  }else{
    disable("seed")
  }
})

#8-Starting experimentation----
##8.1-Reactive value to store models to build by each training vector----
sf$modelstable <- NULL
##8.2-Action when starting experimentation----
observeEvent(input$startexperimentation,{
  #Trigger an alert if:
  #   There is no a ts selected
  #   There is no a scale selected
  #   Have not added a temporal horizon
  #   There is no a an input amount for training vectors selected
  #   There is no an amount of LSTM layers per model selected
  #   There is no a neurons per LSTM layer selected
  #   There is not an epoch amount specified
  if(is.null(input$selectimeseries) || is.null(input$selectimeseriescales) ||
     is.na(input$temporalhorizon) || is.null(input$selectinputoptions) ||
     is.null(input$selectLSTMsoptions) || is.null(input$selectneuronsoptions) ||
     is.na(input$selectepochamount)){
    
    shinyalert(html = T,
               
               text = startalert,
               
               type = 'error')
    
  }else{
    #Trigger an alert if epoch amount does not fulfill conditions
    #   Integer and bigger than 0
    if(input$selectepochamount < 1 || !is.integer(input$selectepochamount)){
      shinyalert("Error",
                 "Wrong epoch format, most be an integer number bigger than 0",
                 type = 'error')
    }else{
      #Trigger an alert if use seed configuration is selected but there is
      #no a seed to use
      if(input$setseed == T && is.na(input$seed)){
        shinyalert("Error",
                   "If a seed is going to be used a seed most be specified",
                   type = 'error')
      }else{
        
        #Show modal of models per training vector where user can delete models
        showModal(
          modalDialog(
            selectmodelstobuild(
              train = database$selectedtrains,
              ts = input$selectimeseries,
              sc = input$selectimeseriescales,
              vec = input$selectinputoptions,
              lstm = input$selectLSTMsoptions,
              neu = input$selectneuronsoptions
            ),
            footer = NULL,
            size = "l"
          )
        )
        
        #Store the models to use
        sf$modelstable <- findmodels(
          input$selectLSTMsoptions,
          input$selectneuronsoptions)
        
      }
    }
    
  }
  
})

##8.3-Models table----
###8.3.1-Updating models table, text and ok button when user modify the amount of model----
observeEvent(c(input$startexperimentation,sf$modelstable),{
  
  if(!is.null(sf$modelstable)){
    
    #Table
    output$modelestable <- renderDT({
      datatable(sf$modelstable,
                options = list(
                  dom = "t",
                  pageLength = dim(sf$modelstable)[1]
                ))
    })
    
    #Text
    amountofts <- dim(database$selectedtrains)[1] * length(input$selectimeseries) * length(input$selectimeseriescales)
    amountofvec <- amountofts * length(input$selectinputoptions)
    
    if(dim(sf$modelstable)[1] == 1){
      modelors <- "model"
    }else{
      modelors <- "models"
    }
    
    output$modelstobuildtext <- renderUI({
      p("Therefore there are goin to be use",
        as.character(amountofvec),
        "vectors to build ",
        as.character(dim(sf$modelstable)[1] * amountofvec),
        "models, ",as.character(dim(sf$modelstable)[1])," ",
        modelors," per vector.",
        style = "color: black")
    })
    
    #Ok button
    if(dim(sf$modelstable)[1] == 0){
      disable(selector = '#acceptmodels')
    }
    
  }
  
})

###8.3.2-Eliminating options of select models table----
observeEvent(input$eliminatemodel,{
  rwstr <- input$modelestable_rows_selected
  sf$modelstable <- sf$modelstable %>%
    slice(-rwstr)
  
})

###8.3.3-Canceling models table modal----
observeEvent(input$cancelmodels,{
  removeModal()
})
#9-Acepting models to build----
##9.1-Reactive value to store copy of models to build----
sf$modelstablecopy <- NULL
##9.2-Accepting models to build----
observeEvent(input$acceptmodels,{
  removeModal()
  #To copy Xdata
  sf$Xdata <- database$Xdata
  #To copy grid table
  sf$grid <- hot_to_r(input$io_gridtable)
  #To copy selected variables
  sf$EDA <- database$EDA
  #To copy selected trains
  sf$selectedtrains <- database$selectedtrains
  #To copy test start
  sf$teststart <- input$selectteststart
  #To copy test end
  sf$testend <- input$selecttestend
  #To copy ts transform
  sf$tstransform <- input$selectimeseries
  #To copy ts scales
  sf$tsscales <- input$selectimeseriescales
  #To copy temporal horizon
  sf$tmph <- input$temporalhorizon
  #To copy input amounts
  sf$inpvec <- input$selectinputoptions
  #To copy epoch
  sf$epoch <- input$selectepochamount
  #To copy if set seed
  sf$setseed <- input$setseed
  #To copy seed
  sf$seed <- input$seed
  #Copying models
  sf$modelstablecopy <- sf$modelstable
  
  
  #To show repeat experimentation button
  show("repeatexperimentation")
  click('repeatexperimentation')
  
})