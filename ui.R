library(shiny)
library(shinyWidgets)

#Inloggen


Dat <- tibble(group = c(rep("A", 3), rep("B", 3), rep("C", 3)),
              x = rep(1:3, 3),
              y = c(2,3,4,14,13,12,23,22,24))


navbarPage(
  
  "Beter Bodembeheer", 
  id = "tabs",
  
  tabPanel(
    "login",
    sidebarPanel(
      textInput(
        "user", 
        "User name"
        ),
      passwordInput(
        "PW", 
        "Password"
        ),
      actionButton(
        "button", 
        "Log in"
        ),
      textOutput(
        "modus"
        )
      )
    ),

  tabPanel(
    
    "Bodemlab-data",
    sidebarPanel(
      
      selectInput(
        inputId = "BOERID", 
        label = "Agrariër",
        choices = unique(
          df_lab$BOERID
          )
        ),
      
      pickerInput(
        inputId = "perceel",
        label = "Perceel",
        choices = unique(
          df_lab$Perceel
          ),
        options = list(
          "actions-box" = TRUE
          ),
        multiple = TRUE
        ),
      
      selectInput(
        inputId = "parameter",
        label = "Meting",
        choices = names(
          df_lab
          )
        ),
      
      sliderInput(
        inputId = "datumrange", 
        label = "Datumrange:",
        min = min(
          df_lab$Datum
          ),
        max = max(
          df_lab$Datum
          ),
        value = c(
          min(
            df_lab$Datum
            ),
          max(
            df_lab$Datum
            )
          )
        )
      ),
      
    mainPanel(
      girafeOutput(
        "bodemdata", height = "800"
        )
      )
    ),
  
  tabPanel(
    "Sensordata",
    sidebarPanel(
      selectInput(
        inputId = "BOERID_sen", 
        label = "Agrariër",
        choices = unique(
          df_beheer$BOERID
          )
        ),
      selectInput(
        inputId = "sensorpar",
        label = "sensormeting",
        choices = c(
          "EC", 
          "Temp", 
          "Bodemvocht", 
          "O2", 
          "pH"
          )
        ),
      sliderInput(
        inputId = "datumrange_perceel", 
        label = "Datumrange:",
        min = min(
          df_sensor$datetime
          ),
        max = max(
          df_sensor$datetime
          ),
        value = c(
          min(
            df_sensor$datetime
            ),
          max(
            df_sensor$datetime
            )
          )
        )
      ),
    mainPanel(
      girafeOutput(
        "handelingen", height= "1000"
        )
      )
    ),
  
  tabPanel(
    "Onderzoek",
    sidebarPanel(
      pickerInput(
        inputId = "BOERID_res",
        label = "BOERID",
        choices = unique(
          df_lab$BOERID
          ),
        selected = unique(
          df_lab$BOERID
          ),
        options = list(
          "actions-box" = TRUE
          ),
        multiple = TRUE
        ),
      selectInput(
        inputId = "y",
        label = "Y-as",
        choices = names(
          df_lab
          )
        ),
      selectInput(
        inputId = "x",
        label = "X-as",
        choices = names(
          df_lab
          )
        ),
      sliderInput(
        inputId = "datumrange_res", 
        label = "Datumrange:",
        min = min(
          df_lab$Datum
          ),
        max = max(
          df_lab$Datum
          ),
        value = c(
          min(
            df_lab$Datum
            ),
          max(df_lab$Datum
              )
          )
        )
      ),
    mainPanel(
      girafeOutput(
        "scatterplot_res"
        )
      )
    )
  )

