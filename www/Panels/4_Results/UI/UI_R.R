div(id = "Thirddiv",
    style = "position: relative; margin:1em 0 0 0",
    absolutePanel(
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
      div(
        id = "dragablepanelcontent",
        div(tags$b("Download")),
        checkboxInput(
          "downloaddash",
          "Dashboard",
          value = F
        ),
        pickerInput(
          "downloadmodels",
          "Models",
          choices = "",
          selected = "",
          multiple = T,
          options = pickerOptions(
            style = "btn-default",
            size = 3,
            `live-search` = T,
            `actions-box` = T)
        ),
        div(
          downloadButton(
            "acceptdownload",
            label = ""),
          style = "text-align: center"
        )
      ),
      draggable = T
    ),
    tabsetPanel(id = "results_tabset")
    )