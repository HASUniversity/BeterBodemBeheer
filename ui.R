library(shiny)
library(shinyWidgets)


navbarPage("Beter Bodembeheer",

  tabPanel("Bodemlab-data",
    sidebarPanel(
      selectInput(inputId = "BOERID", 
                  label = "Agrariër",
                  choices = unique(df_lab$BOERID)),
      pickerInput(
        inputId = "perceel",
        label = "Perceel",
        choices = unique(df_lab$Perceel),
        options = list("actions-box" = TRUE),
        multiple = TRUE,
        ),
      selectInput(
        inputId = "parameter",
        label = "Meting",
        choices = names(df_lab)
      ),
      sliderInput(inputId = "datumrange", 
                  label = "Datumrange:",
                  min = min(df_lab$Datum),
                  max = max(df_lab$Datum),
                  value = c(min(df_lab$Datum),
                            max(df_lab$Datum))
                  )
      ),
    mainPanel(
      plotOutput("bodemdata", hover = "bodemdata_hover"),
      verbatimTextOutput("perceelnaam"),
      plotOutput("bodemdata_mean")
    )
  ),
  
  tabPanel("Sensordata",
    sidebarPanel(
      selectInput(inputId = "BOERID_sen", 
                  label = "Agrariër",
                  choices = unique(df_beheer$BOERID)),
      selectInput(
        inputId = "sensorpar",
        label = "sensormeting",
        choices = c("EC", "Temp", "Bodemvocht", "O2", "pH")
        ),
      sliderInput(
        inputId = "datumrange_perceel", 
        label = "Datumrange:",
        min = min(df_sensor$datetime),
        max = max(df_sensor$datetime),
        value = c(min(df_sensor$datetime),
                  max(df_sensor$datetime))
        )
      ),
    mainPanel(
      plotOutput("sensordata"),
      plotOutput("Neerslag"),
      plotOutput("sensormean")
    )
  ),
  
  tabPanel("Onderzoek",
    sidebarPanel(
      pickerInput(
        inputId = "BOERID_res",
        label = "BOERID",
        choices = unique(df_lab$BOERID),
        selected = unique(df_lab$BOERID),
        options = list("actions-box" = TRUE),
        multiple = TRUE,
        ),
      selectInput(
        inputId = "y",
        label = "Y-as",
        choices = names(df_lab)
        ),
      selectInput(
        inputId = "x",
        label = "X-as",
        choices = names(df_lab)
      ),
      sliderInput(inputId = "datumrange_res", 
                  label = "Datumrange:",
                  min = min(df_lab$Datum),
                  max = max(df_lab$Datum),
                  value = c(min(df_lab$Datum),
                            max(df_lab$Datum))
                  )
    ),
    mainPanel(
      plotOutput(
        "scatterplot_res",
        hover = "scatter_hover"
        ),
      #verbatimTextOutput("scatter_info"),
      verbatimTextOutput("scatter_info"),
    )
  )
)

