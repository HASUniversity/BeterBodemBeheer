server <- function(session, input, output) {
  
  hideTab(inputId = "tabs", 
          target = "Bodemlab-data")
  hideTab(inputId = "tabs", 
          target = "Sensordata")
  hideTab(inputId = "tabs", 
          target = "Onderzoek")
  
  
 # login
  
  status <- eventReactive(
    input$button, {
      if (!input$user %in% df_login$Username) {
        return(0)
        } 
      else if (checkpw(input$PW, 
                       hashpw((df_login %>% filter(Username == input$user))$Password))) {
        return(1)
        }
      else {
        return(2)
      }
  })
  
  
  
  modus <- reactive({
    if(status() == 0) {
      return("User name not known, contact m.smits@has.nl")
    }
    else if (status() == 2) {
      return("Wrong Password, try again or contact m.smits@has.nl")
    }
    else {
      return(paste0("Ingelogd als ", input$user))
    }
  })

  output$modus <- renderText(
     paste0("Status: ", modus())
  )

  
  
# Add the right BOERID in the choice list
observe({
  if(status() == 1) {
    
    if(input$user == "RES") {
      print("check boerid")
      x <- unique(df_lab$BOERID)
      x_sen <- unique(df_beheer$BOERID) 
      showTab(inputId = "tabs",
              target = "Onderzoek")
    }
    
    else {
      x <- input$user
      x_sen <- input$user
    }
    
    updateSelectInput(
      session,
      inputId = "BOERID",
      choices =  x
      )
    
    updateSelectInput(
      session,
      inputId = "BOERID_sen",
      choices =  x_sen
      )
    
    showTab(
      inputId = "tabs", 
      target = "Bodemlab-data"
      )
    showTab(
      inputId = "tabs", 
      target = "Sensordata"
    )
    }
})  

  
  
 observe({
   priority = 9
   x <- input$BOERID
   updatePickerInput(
     session,
     inputId = "perceel",
     choices = get_perceellist(x)
   )
 })
 
 observe({
   priority = 8
   x <- input$BOERID
   y <- input$perceel
   updateSelectInput(
     session,
     inputId = "parameter",
     choices = get_parameterlist(x,y)
     )
   print("update parameterlijst")
 })
 
  observe({
    x <- input$BOERID_res
    updatePickerInput(
      session,
      inputId = "perceel_res",
      choices = get_perceellist(x)
    )
    print("update perceel res")
  })
  

  observe({
    x <- input$BOERID
    updatePickerInput(session,
                      inputId = "perceel_sensor",
                      choices = get_perceellist(x))
    print("update perceel sensor")
  })   
  
  
  observe({
    updateSelectInput(
      session,
      inputId = "x",
      choices = get_xvar(input$BOERID_res, input$y)
    )
    print("update xy research")
  })   

  
  #Figuren
  
  output$bodemdata <- renderGirafe(
    girafe(ggobj = plot_bodemdata(
      input$perceel,
      input$datumrange,
      input$parameter)
    )
  )
  
  
  output$handelingen <- renderGirafe({
    girafe(ggobj = plot_grid(
      plot_handeling(
        input$BOERID_sen,
        input$datumrange_perceel
        ),
      plot_sensordata(
        input$BOERID_sen,
        input$datumrange_perceel,
        input$sensorpar
        ),
      plot_neerslag(
        input$BOERID_sen,
        input$datumrange_perceel
        ),
      plot_sensormean(
        input$datumrange_perceel,
        input$sensorpar),
      ncol = 1, align = "v"),
      height_svg = 2*5,
      width_svg = 6
      )
  }
  )
  
 output$scatterplot_res <- renderGirafe({
   girafe(ggobj = plot_scatter_res(input$BOERID_res,
                           input$datumrange_res,
                           input$x,
                           input$y))
 }
 ) 
  
}



