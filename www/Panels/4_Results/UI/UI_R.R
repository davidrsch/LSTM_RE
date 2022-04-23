#USER INTERFACE----
#RESULTS PANEL----
#Container----
div(
  id = "Thirddiv",
  style = "position: relative; margin:1em 0 0 0",
  ##1-Download----
  #Dragable panel to download the resulting dashboards once finished
  #the experimentations
  absolutePanel(
    ###1.1-Collapse button----
    div(
      div(
        actionButton(
          "collapseDragDiv",
          "",
          icon = icon(
            'window-minimize',
            style = "font-size: 19px;
                     border: thin solid;
                     padding: 1%;",
            id = "collapseDragDivicon")
        ),
        style = "margin-bottom: 2%; float:right;"
      ),
      style = "display: flow-root;"
    ),
    ###1.2-Download button----
    div(
      id = "dragablepanelcontent",
      pickerInput(
        "downloadmodels",
        "Download:",
        choices = "",
        selected = NULL,
        multiple = T,
        options = pickerOptions(
          style = "btn-default",
          size = 3,
          `live-search` = T,
          `actions-box` = T)
      ),
      ###1.3-Accept download----
      div(
        downloadButton(
          "acceptdownload",
          label = ""),
        style = "text-align: center"
      )
    ),
    draggable = T
  ),
  ##2-Results Tabset Panel----
  tabsetPanel(id = "results_tabset")
)