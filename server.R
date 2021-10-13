server <- function(session, input, output) {
  
  hideTab(inputId = "tabs", 
          target = "Bodemlab-data")
  hideTab(inputId = "tabs", 
          target = "Sensordata")
  hideTab(inputId = "tabs", 
          target = "Onderzoek")
  
  
  
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
  
 observe({
    if(status() == 1 & input$user != "RES") {
     updatePickerInput(
       session,
       inputId = "BOERID",
       choices = input$user
     )
    showTab(inputId = "tabs", 
            target = "Bodemlab-data")
    showTab(inputId = "tabs", 
            target = "Sensordata")
    }
 }) 
  
 observe({
    if (status() == 1 & input$user == "RES") {
      
     # updatePickerInput(
     #   session,
     #   inputId = "BOERID",
     #   choices = unique(df_lab$BOERID)
     # )
    showTab(inputId = "tabs", 
            target = "Bodemlab-data")
    showTab(inputId = "tabs", 
            target = "Sensordata")
    showTab(inputId = "tabs",
            target = "Onderzoek")
    }
 })
 
 # observe({
 #   if(input$user != "RES") {
 #     updatePickerInput(
 #       session,
 #       inputId = "BOERID",
 #       choices = input$user
 #     )
 #   }
   # else if (input$user == "RES") {
 #   else {
 #     updatePickerInput(
 #       session,
 #       inputId = "BOERID",
 #       choices = unique(df_lab$BOERID)
 #     )
 #   } 
 # })
 
  observe({
    x <- input$BOERID
    updatePickerInput(
      session,
      inputId = "perceel",
      choices = get_perceellist(x)
    )
  })   

  observe({
    updateSelectInput(
      session,
      inputId = "parameter",
      choices = get_parameterlist(input$BOERID))
  })   

  observe({
    updateSelectInput(
      session,
      inputId = "parameter",
      choices = get_parameterlist(input$BOERID))
  })   
  observe({
    x <- input$BOERID
    updatePickerInput(session,
                      inputId = "perceel_sensor",
                      choices = get_perceellist(x))
  })   
  
  observe({
    updateSelectInput(
      session,
      inputId = "y",
      choices = get_parameterlist(input$BOERID_res)
    )
  })   
  
  observe({
    updateSelectInput(
      session,
      inputId = "x",
      choices = get_xvar(input$BOERID_res, input$y)
    )
  })   

  #Figuren
  
  output$bodemdata <- renderGirafe({
      girafe(ggobj = plot_grid(
        plot_bodemdata(
          input$perceel, 
          input$datumrange,
          input$parameter
          ),
        plot_bodemdata_mean(
          input$datumrange,
          input$parameter),
        ncol = 1, align = "v"),
        height_svg = 2*5,
        width_svg = 6
        )
   }
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
  # output$scatterplot_res <- renderPlot(
  #   df_lab %>% 
  #     filter(BOERID %in% input$BOERID_res) %>% 
  #     filter(Datum > input$datumrange_res[1],
  #            Datum < input$datumrange_res[2]) %>% 
  #     ggplot(aes(!!sym(input$x), !!sym(input$y))) +
  #     geom_point(aes(color = BOERID), size = 3) +
  #     geom_smooth(method = "lm")
  # )
  # 
  # get_row_scatter_info <- reactive({
  #   req(input$scatter_hover)
  #   
  #   l <- nearPoints(
  #     df_lab %>% 
  #       filter(BOERID %in% input$BOERID_res) %>% 
  #       filter(Datum > input$datumrange_res[1],
  #              Datum < input$datumrange_res[2]),
  #     input$scatter_hover,
  #     threshold = 10,
  #     maxpoints = 1
  #   )
  #   return("test")
  #   
  # })

  # output$scatter_info <- renderPrint({
  #   req(get_row_scatter_info())
  #   get_row_scatter_info()
      # l <- nearPoints(
      #   df_lab %>% 
      #     filter(BOERID %in% input$BOERID_res) %>% 
      #     filter(Datum > input$datumrange_res[1],
      #            Datum < input$datumrange_res[2]),
      #   input$scatter_hover,
      #   threshold = 10
      # )
      # paste(c(l$Datum, l$BOERID, l$Perceel), sep = " ")
      # ) %>% 
      # select(Datum, BOERID, Perceel)
  # })
  
}



