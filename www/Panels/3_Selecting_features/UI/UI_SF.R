
div(id = "Seconddiv",
    style = "position: relative; margin:1px -1em 0 -1em",
    
    div(id = "SSBL",
        
        sidebarLayout(
            
            #INPUTS
            sidebarPanel(
                
                div(style = "height:20%"),
                
                #Time series transformations dropdown
                dropdown(
                    inputId = "ts_and_scales_dd",
                    label = "Time series transformations",
                    
                    #Time series picker transformations
                    pickerInput(
                        "selectimeseries",
                        "Time series to use",
                        choices = list("Original","First transformation",
                                       "Second transformation"),
                        selected = list("Original","First transformation",
                                        "Second transformation"),
                        multiple = T,         
                        options = pickerOptions(
                            title = "Select at least one time serie",
                            header = "Select at least one time serie",
                            style = "btn-default")),
                    
                    #Scales picker
                    pickerInput(
                        "selectimeseriescales",
                        "Scales to use",
                        choices = list("Exact","From 0 to 1","From -1 to 1"),
                        selected = list("Exact","From 0 to 1","From -1 to 1"),
                        multiple = T,         
                        options = pickerOptions(
                            title = "Select at least one scale",
                            header = "Select at least one scale",
                            style = "btn-default")),
                    
                    #Div to show if there is any time serie with values equal or
                    #smaller than 0.
                    div(id = "valuesequalorless0",
                        p("Some of the choosen time series has values equal or",
                          " smaller than 0, therefore the 2nd transformation",
                          " can't be applyed",
                          style = "color: red")
                    ),
                    #div to show if any of the selected time series is stationary
                    div(id = "Stationaryts",
                        p("Some of the chossen time series are already stationaries",
                          ", therefore the transformations won't be applyed",
                          style = "color: red"))
                    
                ),
                
                #Space between Time seriesscales and training vectors dropdowns
                div(style = "height:1.2em"),
                
                #Training vectors options dropdown
                dropdown(
                    inputId = "tv_opt_dd",
                    label = "Training vectors options",
                    
                    #Numeric input to temporal horizon     
                    numericInput(
                        "temporalhorizon",
                        "Specify the temporal horizon",
                        value = NULL,
                        min = 1),
                    
                    #Numeric input to option of an input amount
                    numericInput(
                        "addINoption",
                        "Add input amount",
                        value = NULL,
                        min = 1),
                    
                    #To accept input amount
                    actionButton(
                        "acceptinputoptionbutton",
                        "Add input"),
                    
                    #Space between accept and select inputs amount
                    div(style = "height:1.2em"),
                    
                    #Select inputs amounts     
                    pickerInput(
                        "selectinputoptions",
                        "Select the amounts of inputs",
                        choices = NULL,
                        selected = NULL,
                        multiple = T,
                        options = pickerOptions(
                            title = "Select at least one input amount",
                            header = "Select at least one input amount",
                            style = "btn-default",
                            size = 3))
                         
                         
                ),
                
                #Space between training vectors and models dropdowns
                div(style = "height:1.2em"),
                
                #Models options dropdown
                dropdown(
                    inputId = "models_opt_dd",
                    label = "Models options",
                    
                    #To add a number of LSTM layers
                    numericInput(
                        "addLSTMamount",
                        "Add LSTM layer amount",
                        value = NULL,
                        min = 1),
                    
                    #To accept number of LSTM layers
                    actionButton(
                        "acceptLSTMamountbutton",
                        "Add amount"),
                    
                    #Space between accept and select amounts of LSTM layers
                    div(style = "height:1.2em"),
                    
                    #Select LSTM layers
                    pickerInput(
                        "selectLSTMsoptions",
                        "Select the amounts of LSTM",
                        choices = NULL,
                        selected = NULL,
                        multiple = T,         
                        options = pickerOptions(
                            title = "Select at least one LSTM amount",
                            header = "Select at least one LSTM amount",
                            style = "btn-default",
                            size = 3)),
                    
                    #To add a number of neurons     
                    numericInput(
                        "addneuronsamount",
                        "Add neuron amount",
                        value = NULL,
                        min = 1),
                    
                    #To accept a number of neurons
                    actionButton(
                        "acceptneuronamountbutton",
                        "Add amount"),
                    
                    #Space between accept and select number of neurons     
                    div(style = "height:1.2em"),
                    
                    #Select number of neurons     
                    pickerInput(
                        "selectneuronsoptions",
                        "Select the amounts of neurons",
                        choices = NULL,
                        selected = NULL,
                        multiple = T,         
                        options = pickerOptions(
                            title = "Select at least one neurons amount",
                            header = "Select at least one neurons amount",
                            style = "btn-default",
                            size = 3))
                ),
                
                #Space between Models and training dropdowns
                div(style = "height:1.2em"),
                
                #Training options
                dropdown(
                    inputId = "training_opt_dd",
                    label = "Training options",
                    
                    #Explanation of used Batch size     
                    strong("Batch size"),
                    div("In order to keep the experiments simple, only",
                        strong("stochastic gradient descent"),"is used.",
                        style = "text-align: initial; margin: 0 0 1em 0"),
                    
                    #Specify epoch amount     
                    numericInput(
                        "selectepochamount",
                        "Epoch",
                        value = NULL,
                        min = 1),
                    
                    #Div containing seed options
                    div(
                        #To select if use or not seed
                        checkboxInput(
                            "setseed",
                            tags$b("Set seed"),
                            value = T),
                        
                        div(style = "width:10%"),
                        
                        #Seed to use
                        numericInput(
                            "seed",
                            "Seed",
                            value = 123),
                        
                        #Style of the div containing seed
                        style = "display: flex;
                        justify-content: space-between"
                    ),
                    up = T
                         
                ),
                
                div(style = "height:1.2em"),
                
                #To start experimentation
                actionButton(
                    "startexperimentation",
                    "Start",
                    style = "width: 50%;"
                ),
                
                div(style = "height:1.2em"),
                
                #To repeat experimentation
                actionButton(
                    "repeatexperimentation",
                    "Repeat",
                    style = "width:50%"
                ),
                
                #Style of sidebar panel
                style = "background-color: white;
                           border-color:black;
                           border-radius:0;
                           text-align:center;
                           height: 500px"
                
            ),
            
            #2. - OUTPUTS
            mainPanel(
                
                wellPanel(
                    
                    tabsetPanel(
                        
                        id = "Proposal_tabsetPanel",
                        
                        #Time series and scales options
                        tabPanel(
                            "Time series",
                            
                            tags$embed(
                                type = "text/html",
                                src = "Panels/3_Selecting_features/UI/1.time_series.html",
                                width = "100%",
                                height = "100%"),
                            
                            value = "TS_and_scales_opt_tabPanel",
                            style = "height:430px"),
                        
                        #Training vectors options
                        tabPanel(
                            "Training vectors",
                            
                            tags$embed(
                                type = "text/html",
                                src = "Panels/3_Selecting_features/UI/2.training_vectors.html",
                                width = "100%",
                                height = "100%"),
                            
                            value = "Training_vectors_tabPanel",
                            style = "height:430px"),
                        
                        #Neurons in the LSTM layer
                        tabPanel(
                            "Models",
                            
                            tags$embed(
                                type = "text/html",
                                src = "Panels/3_Selecting_features/UI/3.models.html",
                                width = "100%",
                                height = "100%"),
                            
                            value = "Moedels_tabPanel",
                            style = "height:430px"),
                        
                        #Training and testing options
                        tabPanel(
                            "Training",
                            tags$embed(
                                type = "text/html",
                                src = "Panels/3_Selecting_features/UI/4.training.html",
                                width = "100%",
                                height = "100%"),
                            value = "Training_options_tabPanel",
                            style = "height:430px"),
                        
                    ),
                    
                    #Style of the well panel inside the main panel
                    style = "background-color: white;
                        border-color:black;border-radius:0;
                        height:100%"
                    
                ),
                
                #Main Panel style
                style = "height:500px"
                
            )
            
        ),
        
        #Style the content of the FSBL content
        style = "background-color: white;
            padding: 1em 1em 0 1em"
        
    )
    
)
