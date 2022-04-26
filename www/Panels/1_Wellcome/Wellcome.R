#USER INTERFACE----
#WELLCOME PANEL----
tagList(
  #Content of the Panel----
  fluidRow(
    ##Left side (empty)----
    column(3),
    ##Center----
    column(
      width = 6,
      div(
        ###About app----
        tags$p("Power by",
               style = "font-size: 1.5em; color:black; margin: .5em 0 .3em 0;
               text-align:center"),
        div(tags$img(src = "logos/R_logo.png", style = "width: 4em; height: 4em;
                 margin-right:1em"),
            tags$img(src = "logos/Keras_logo.png", style = "width: 4em; height: 4em;
                 margin-left:1em"),
            style = "text-align:center"),
        div(
          tags$p("App to carry out simple experiments of the use of LSTM networks in",
                 " time series forecasting. It allows users to solves univariate and",
                 " multivariate regression problems by testing different set of features",
                 " and comparing the effectiviness of the obtained results."),
          tags$p("Features that allows configurations are:"),
          tags$ul(
            tags$li("Training set size."),
            tags$li("Time series transformation.(restricted to the ones offered by the",
                    " author)"),
            tags$li("Input vector size."),
            tags$li("LSTM layers amount."),
            tags$li("LSTM layers' neurons amount."),
            tags$li("Epoch amount."),
            tags$li("Use or not use of seed number and the seed number to use.")
          ),
          tags$p("At the end of each experiment, users can download the obtained",
                 " models and the resulting interactive dashboard."),
          style = "font-size: 16px; color: black; margin-top: 1%;"
        ),
        ###Credits of app----
        tags$p("Created by",
               style = "font-size: 1.5em; color:black; margin: .5em 0 .3em 0"),
        tags$p("David Díaz Rodríguez",
               style = "font-size: 1.7em; color:black; margin:0"),
        ####Contact info----
        #####01-Email----
        tags$a(href = "mailto:daviddrsch@gmail.com",
               tags$img(src = "logos/Gmail_logo.png",
                        style = "width: 2em; height: 2em"),
               style = "margin-right: .5em"),
        #####02-Github----
        tags$a(href = "https://github.com/davidrsch",
               tags$img(src = "logos/GitHub_logo.png",
                        style = "width: 2em; height: 2em")),
        #####03-LinkedIn----
        tags$a(href = "https://www.linkedin.com/in/david-d-6257951b8",
               tags$img(src = "logos/Linkedin_logo.png",
                        style = "width: 2.1em; height: 2em; margin-left:.4em")),
        #####04-Stackoverflow----
        tags$a(href = "https://stackoverflow.com/users/12660035/",
               tags$img(src = 'logos/Stackoverflow_logo.png',
                        style = "width: 2.1em; height: 2em; margin-left:.4em")),
        #####05-ORCid----
        tags$a(href = "https://orcid.org/0000-0002-0927-9795",
               tags$img(src = 'logos/ORCIDiD_logo.png',
                        style = "width: 2.1em; height: 2em; margin-left:.4em")),
        tags$br(),
        tags$br(),
        style = "border:1px solid black; margin: 15px 0 0 0; padding: 14px; height:500px;
        overflow: auto"
      )
    ),
    ##Right side (empty)----
    column(3)
  )
)