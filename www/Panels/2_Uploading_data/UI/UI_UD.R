#USER ITERFACE ----
#UPLOADIN DATA PANEL----
#Container----
div(
  id = "Firstdiv",
  style = "position: relative; margin:1px -1em 0 -1em",
  
  ## Sidebar layout----
  div(
    id = "SBL1",
    sidebarLayout(
      
      ###INPUTS(SBP)----
      sidebarPanel(
        fluid = T,
        
        #### 1-To upload file----
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
              <div class="progress-bar">
            </div>
          </div>
        </div>'),
        ####2-Imported file options----
        #Imported file options dropdown
        dropdown(
          inputId = "imported_DD",
          label = 'Imported file',
           
          #####2.1-If the dataframe has header or not----
          prettyCheckbox(
            "header",
            "Header",
            T,
            icon = icon("check")),
           
          #####2.2-To select the delimiter for the csv or tsv files----
          pickerInput(
            inputId = "delimiter",
            label = "Select the delimiter",
            c(
              "Comma" = ",",
              "Semicolon" = ";",
              "Tab" = "\t",
              "Whitespace" = " ")),
           
          #####2.3-To select the decimal mark in csv or tsv files----
          pickerInput(
            inputId = "dm",
            label = "Select the decimal mark",
            c(
              "." = ".",
              "," = ","))
        ),
         
        #Space between the 1st and 2nd dropdown
        div(style = "height:1.2em"),
         
        ####3-Select the date variable and the variable to forecast----
        #Select variables dropdown
        dropdown(
          inputId = "selectvariables",
          label = "Select variables",
           
          #####3.1-Select the date-sequence variable----
          pickerInput(
            "datevariable",
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
           
          #####3.2-Dropdown containing div of io variables----
          dropdown(
            inputId = "input_output_grid",
            label = "Select variables",
            up = T,
            #io grid table
            rHandsontableOutput("io_gridtable")
          ),
           
          #Space between the io grid ddown and the select variables ddown
          div(style = "height: 0.5em")
          
        ),
         
        #Space between the 2nd and 3rd dropdown
        div(style = "height:1.2em"),
         
        ####4-Select amount of data to use dropdown----
        dropdown(
          inputId = "selectamounttouse",
          label = "Select the data to use",
          up = T,
          
          #####4.1-Test set----
          div(
            tags$strong(
              "Test set:",
              style = "margin:auto"),
            style="text-align:center"),
           
          div(
            ######4.1.1-Start Test set----
            #Picker input to select the value from the
            #date-sequence variable of the start test date
            pickerInput(
              inputId = "selectteststart",
              label = "Start",
              choices = NULL,
              selected = NULL,
              options = pickerOptions(
                liveSearch = TRUE,
                size = 3,
                style = "btn-default")),
             
            div(style = 'width:10px'),
            ######4.1.2-End Test set----
            #Picker input to select the value from the
            #date-sequence variable of the end test date
            pickerInput(
              inputId = "selecttestend",
              label = "End",
              choices = NULL,
              selected = NULL,
              options = pickerOptions(
                liveSearch = TRUE,
                size = 3,
                style = "btn-default"))
          ),
          #####4.2-Start Train set----
          div(
            tags$strong(
              "Train set:",
              style = "margin:auto"),
            style="text-align:center"),
           
          div(
            #Picker input to select the value from the
            #date-sequence variable of the start train date
            pickerInput(
              inputId = "selecttrainstart",
              label = "Start",
              choices = NULL,
              selected = NULL,
              options = pickerOptions(
                liveSearch = TRUE,
                size = 3,
                style = "btn-default")),
            style = "display:block"),
          #####4.3-Ad dates----
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
         
        div(
          id = "containerofstd",
          ####5-Table containing the selected start train values----
          DTOutput("traindatestable"),
          style = "border-radius: 0;
          border: black thin solid;
          max-height: 140px;
          overflow: auto;
          font-size: 0.9em;
          text-align: center"),
         
        div(style = "height:1.2em"),
         
        ####6-Button to eliminate vales from the table containing the----
        #selected start train values
        actionButton(
          "eliminatetrainsd",
          "Eliminate",
          style = "float: right;"
        ),
         
        ###STYLE OF INPUTS(SBP)----
        style = "background-color: white;
                 border-color:black;
                 border-radius:0;
                 text-align:center;
                 height: 500px"
         
      ),
        
      ###OUTPUTS(MP&WP)----
      mainPanel(
          
        wellPanel(
            
          ####1-Starting application and on error of uploading file----
          uiOutput("fileupload"),
            
          ####2-Tabset panel containg outputs----
          #  * Imported data
          #  * EDA
          #  * Graphs of selected variables and periods
          tabsetPanel(
            id="tableandgraphs",
              
            #####2.1-Imported data tab panel----
            tabPanel(
              title = "Data",
              
              ######2.1.1-Imported file name----
              uiOutput("filename"),
                
              ######2.1.2-Imported file table----
              dataTableOutput("files")
            ),
              
            #####2.2-EDA tab panel----
            tabPanel(
              title = "EDA",
                
              ######2.2.1-Glide containing a visual EDA and a summary EDA----
              glide(
                id ="summaryandgraph",
                
                ######a)-Visual Exploratory Data Analysis screen of the glide----
                screen(
                   
                  #Loading
                  withSpinner(
                    #Visual EDA
                    ui_element = plotOutput(
                      "eda",
                      height = "350px"),
                    id = "loadingeda")
                ),
                 
                ######b)-Summary Exploratory Data Analysis screen of the glide----
                screen(
                  #Summary of data
                  div(
                    id = "summaryandtitle",
                    tags$h4("Summary"),
                    align = "center",
                    style = "color:black",
                    #Loading
                    withSpinner(
                      #Summary table EDA
                      dataTableOutput(
                        "summary",
                        height = 300),
                      id = "loadingsummary")
                  )
                )
              ),
              style = "padding-top:1em"
               
            ),
            #####2.3-Graphs of selected variables and periods tab panel----
            tabPanel(
              title = "Graphs",
              #Selected variables plot grid
              div(
                uiOutput(
                  "plotselectedvariables",
                  style = "margin-left: -1em; margin-right: -1em"),
                style = "margin: 2% 0 0 0;
                        height: 400px; overflow:auto"
              )
             
            )
             
          ),
          class = "flex-center",
           
          ###STYLE OF OUTPUT(WP)----
          style = "background-color: white;
                   border-color:black;border-radius:0;
                   height:100%"
           
        ),
        ###STYLE OF OUTPUT(MP)----
        style = "height:500px"
      )
    ),
    
    ##Style the content of the FSBL content----
    style = "background-color: white;
             padding: 1em 1em 0 1em"
  )
)