server <- function(session, input, output) {

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
  
  output$bodemdata <- renderPlot(
    df_lab %>% 
      filter(Perceel %in% input$perceel) %>% 
      filter(Datum > input$datumrange[1],
             Datum < input$datumrange[2]) %>% 
      ggplot(aes(Datum, !!sym(input$parameter), color = Perceel)) +
      geom_point(size = 3) +
      geom_line() +
      theme(legend.position = "none")
  )

  output$perceelnaam <- renderPrint({
    (paste(
      "Perceelnaam: ", 
      nearPoints(
        df_lab %>% 
          filter(Perceel %in% input$perceel) %>% 
          filter(Datum > input$datumrange[1],
                 Datum < input$datumrange[2]),
        input$bodemdata_hover,
        threshold = 10
      )$Perceel[1]
      )
    ) 
  })
  
    
  output$bodemdata_mean <- renderPlot(
    df_lab %>% 
      filter(Datum > input$datumrange[1],
             Datum < input$datumrange[2]) %>% 
      ggplot(aes(Datum, !!sym(input$parameter))) +
      geom_smooth()
  )
  
  output$sensordata <- renderPlot(
    df_sensor %>% 
      filter(Perceel %in% get_sensorperceellist(input$BOERID_sen)) %>% 
      filter(datetime > input$datumrange_perceel[1],
             datetime < input$datumrange_perceel[2]) %>% 
      ggplot(aes(datetime, !!sym(input$sensorpar), color = Perceel)) +
      geom_line(size = 1, alpha = 0.8) +
      xlim(input$datumrange_perceel[1], input$datumrange_perceel[2]) +
      theme(legend.position = c(0.87, 0.87))
  )
  
  output$Neerslag <- renderPlot(
    df_neerslag %>% 
      filter(boerID  == input$BOERID_sen) %>% 
      filter(Datum > input$datumrange_perceel[1],
             Datum < input$datumrange_perceel[2]) %>% 
      ggplot(aes(Datum, Neerslag)) +
      scale_fill_manual(values = c("regen" = "grey", "sneeuw" = "orange")) +
      geom_col(aes(fill = Soort)) +
      geom_smooth(se = FALSE) +
      theme(legend.position = c(0.87, 0.87))
  )
  
  output$sensormean <- renderPlot(
    df_sensor %>%
      filter(datetime > input$datumrange_perceel[1],
             datetime < input$datumrange_perceel[2]) %>% 
      ggplot(aes(datetime, !!sym(input$sensorpar))) +
      geom_line(aes(group = Perceel), color = "grey") +
      geom_smooth()
  )
  
  output$scatterplot_res <- renderPlot(
    df_lab %>% 
      filter(BOERID %in% input$BOERID_res) %>% 
      filter(Datum > input$datumrange_res[1],
             Datum < input$datumrange_res[2]) %>% 
      ggplot(aes(!!sym(input$x), !!sym(input$y))) +
      geom_point(aes(color = BOERID), size = 3) +
      geom_smooth(method = "lm")
  )
  

  output$scatter_info <- renderPrint({
      nearPoints(
        df_lab %>% 
          filter(BOERID %in% input$BOERID_res) %>% 
          filter(Datum > input$datumrange_res[1],
                 Datum < input$datumrange_res[2]),
        input$scatter_hover,
        threshold = 10
      ) %>% 
      select(Datum, BOERID, Perceel)
  })
  
}



