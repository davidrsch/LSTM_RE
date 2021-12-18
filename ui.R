library(shiny)
library(shinyalert)
library(shinycssloaders)
library(shinyglide)
library(shinyjs)
library(shinyWidgets)
library(abind)
library(dplyr)
library(DT)
library(english)
library(htmlwidgets)
library(jsonlite)
library(keras)
library(Metrics)
library(tensorflow)
library(future)
library(readxl)
library(readr)
library(tools)
library(ggplot2)
library(GGally)
library(pastecs)
library(plotly)
library(promises)
library(rhandsontable)
library(rmarkdown)
library(runner)
library(tseries)
library(scales)
library(zip)


ui <- fluidPage(
  
  useShinyjs(),
  useShinyalert(),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")
  ),
  
########################################APP_DIV#########################################
div(
  #####################################TITLE_DIV########################################
  div(id="titlediv",
    titlePanel(
      title = h1("Experimentation with LSTM networks for Time series forecasting",
                 style = "text-align: center; border: 1px solid black; padding:10px 0 10px 0"),
      windowTitle = "LSTMexperimentation"
    )
    
  ),
  ###################################MAINPANEL_DIV######################################
  div(
    mainPanel(
      tabsetPanel(
        id = "Main_tabsetpanel",
        
        tabPanel(
          title = "Wellcome",
          source(
            file = "www/Panels/1_Wellcome/Wellcome.R",
            encoding = 'UTF-8')$value,
          value = "WELLCOME_TABPANEL"
          ),
        tabPanel(
          title = "Upload Data",
          source(file = "www/Panels/2_Uploading_data/UI/UI_UD.R")$value
                 ),
        tabPanel(
          title = "Selecting Features",
          source(file = "www/Panels/3_Selecting_features/UI/UI_SF.R")$value
          ),
        tabPanel(
          title = "Results",
          source(file = "www/Panels/4_Results/UI/UI_R.R")$value
        )
        ),
        style = "width: 100%; height:100%; background-color: white; padding: 1%"
      ),
    style = "
             margin: 0 -1.0% 0 -1.0%"
    )
  
  )
)

