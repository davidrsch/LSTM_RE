
    div(id = "Firstdiv",
        style = "position: relative; margin:1px -1em 0 -1em",
        
        div(id = "SBL1",
            sidebarLayout(
              
              #INPUTS
              sidebarPanel(fluid = T,
                           
                           #To upload file
                           HTML('
                       <div class="form-group shiny-input-container">
                         <label class="control-label" for="file">
                           <h4 style = "color:black">
                             <strong>Upload database</strong>
                           </h4>
                         </label>
                         <div class="input-group">
                           <label class="input-group-btn input-group-prepend">
                             <span class="btn btn-default btn-file">
                               Browse...
                               <input id="file" name="file" type="file" style="display: none;"/>
                             </span>
                           </label>
                           <input id = "file_text" type="text" class="form-control" placeholder = "No file selected" readonly="readonly"/>
                         </div>
                         <div id="file_progress" class="progress active shiny-file-input-progress">
                           <div class="progress-bar"></div>
                         </div>
                       </div>
                            '),
                           
                           #Imported file options
                           #Imported file options dropdown
                           dropdown(
                               inputId = "imported_DD",
                               label = 'Imported file',
                              
                               # - If the dataframe has header or not
                               prettyCheckbox("header",
                                              "Header",
                                              T,
                                              icon = icon("check")),
                               
                               # - To select the delimiter for the csv or tsv files
                               pickerInput(inputId = "delimiter",
                                           label = "Select the delimiter",
                                           c("Comma" = ",",
                                             "Semicolon" = ";",
                                             "Tab" = "\t",
                                             "Whitespace" = " ")),
                               
                               # - To select the decimal mark in csv or tsv files
                               pickerInput(inputId = "dm",
                                           label = "Select the decimal mark",
                                           c("." = ".",
                                             "," = ","))
                           ),
                           
                           #Space between the 1st and 2nd dropdown
                           div(style = "height:1.2em"),
                           
                           #Select the date variable and the variable to forecast
                           #Select variables dropdown
                           dropdown(
                               inputId = "selectvariables",
                               label = "Select variables",
                               
                               ## - Select the date-sequence variable
                               pickerInput("datevariable",
                                           "Select the date-sequence variable",
                                           choices = NULL,
                                           multiple = T,
                                           selected = 1,
                                           options = pickerOptions(
                                               title = "Select one option",
                                               size = 4,
                                               maxOptions = 1)
                               ),
                               
                               #Space between the select ds variable and the grid to
                               #select io variables
                               div(style = "height: 0.5em"),
                               
                               #Dropdown containing div of io variables
                               dropdown(inputId = "input_output_grid",
                                        label = "Select variables",
                                        up = T,
                                        #io grid table
                                        rHandsontableOutput("io_gridtable")
                                        ),
                               
                               #Space between the io grid ddown and the 
                               #select variables ddown
                               div(style = "height: 0.5em")
                               
                               
                               ),
                           
                           #Space between the 2nd and 3rd dropdown
                           div(style = "height:1.2em"),
                           
                           #Select amount of data to use dropdown
                           dropdown(
                               inputId = "selectamounttouse",
                               label = "Select the data to use",
                               up = T,
                               
                               div(
                                   tags$strong(
                                       "Test set:",
                                       style = "margin:auto"),
                                   style="text-align:center"),
                               
                               div(
                                   #Picker input to select the value from the
                                   #date-sequence variable of the start test date
                                   pickerInput(inputId = "selectteststart",
                                               label = "Start",
                                               choices = NULL,
                                               selected = NULL,
                                               options = pickerOptions(
                                                   liveSearch = TRUE,
                                                   size = 3,
                                                   style = "btn-default")),
                                   
                                   div(style = 'width:10px'),
                                   
                                   #Picker input to select the value from the
                                   #date-sequence variable of the end test date
                                   pickerInput(inputId = "selecttestend",
                                               label = "End",
                                               choices = NULL,
                                               selected = NULL,
                                               options = pickerOptions(
                                                   liveSearch = TRUE,
                                                   size = 3,
                                                   style = "btn-default"))
                               ),
                               
                               div(
                                   tags$strong(
                                       "Train set:",
                                       style = "margin:auto"),
                                   style="text-align:center"),
                               
                               div(
                                   #Picker input to select the value from the
                                   #date-sequence variable of the start train date
                                   pickerInput(inputId = "selecttrainstart",
                                           label = "Start",
                                           choices = NULL,
                                           selected = NULL,
                                           options = pickerOptions(
                                               liveSearch = TRUE,
                                               size = 3,
                                               style = "btn-default")),
                                   style = "display:block"),
                               
                               div(
                                   #Action button to add a new start train value 
                                   actionButton(
                                       "adtraintotest",
                                       "OK"
                                   ),
                                   style = "display:block")
                               ),
                           
                           #Space between 3rd dropdown and table containing the selected
                           #start train values
                           div(style = "height:1.2em"),
                           
                           div(id = "containerofstd",
                               #Table containing the selected start train values
                               DTOutput("traindatestable"),
                               style = "border-radius: 0;
                                       border: black thin solid;
                                       max-height: 140px;
                                       overflow: auto;
                                       font-size: 0.9em;
                                       text-align: center"),
                           
                           div(style = "height:1.2em"),
                           
                           #Button to eliminate vales from the table containing the
                           #selected start train values
                           actionButton(
                               "eliminatetrainsd",
                               "Eliminate",
                               style = "float: right;"
                           ),
                           
                           #Style of sidebar panel
                           style = "background-color: white;
                           border-color:black;
                           border-radius:0;
                           text-align:center;
                           height: 500px"
                           
              ),
              
              #OUTPUTS
              mainPanel(
                
                wellPanel(
                  
                  #Starting application and on error of uploading file
                  uiOutput("fileupload"),
                  
                  #Tabset panel containg outputs
                  #  * Imported data
                  #  * EDA
                  #  * Graphs of selected variables and periods
                  tabsetPanel(
                      id="tableandgraphs",
                      
                      #Imported data tab panel
                      tabPanel(
                          title = "Data",
                          
                          #- Imported file name
                          uiOutput("filename"),
                          
                          #- Imported file table
                          dataTableOutput("files")
                          ),
                      
                      #EDA tab panel
                      tabPanel(
                          title = "EDA",
                          
                          #Glide containing a visual EDA and a summary EDA
                          glide(
                              id ="summaryandgraph",
                              
                              #Visual Exploratory Data Analysis screen of the glide
                              screen(
                                  
                                  #Loading
                                  withSpinner(
                                      #Visual EDA
                                      ui_element = plotOutput("eda",
                                                              height = "350px"),
                                      id = "loadingeda")
                              ),
                              
                              #Summary Exploratory Data Analysis screen of the glide
                              screen(
                                  #Summary of data
                                  div(id = "summaryandtitle",
                                      tags$h4("Summary"),
                                      align = "center",
                                      style = "color:black",
                                      #Loading
                                      withSpinner(
                                          #Summary table EDA
                                          dataTableOutput("summary",
                                                      height = 300),
                                                  id = "loadingsummary")
                                  )
                              )
                          ),
                          style = "padding-top:1em"
                          
                      ),
                      #Graphs of selected variables and periods tab panel
                      tabPanel(
                          title = "Graphs",
                          #Selected variables plot grid
                          div(
                              
                              uiOutput("plotselectedvariables",
                                       style = "margin-left: -1em; margin-right: -1em"),
                              style = "margin: 2% 0 0 0;
                              height: 400px; overflow:auto"
                          )
                          
                      )
                      
                  ),class = "flex-center",
                  
                  #Style of the well panel inside the main panel
                  style = "background-color: white;
                  border-color:black;border-radius:0;
                  height:100%"
                  
                ),
                style = "height:500px"
              )
            ),
            
            #Style the content of the FSBL content
            style = "background-color: white;
            padding: 1em 1em 0 1em"
        )#,
        #div(
        #  style = "width:100%; clear: both; display: inline-block; height:0.01em")
    )