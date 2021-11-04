library(readxl)
# library(tidyverse)
library(ggplot2)
theme_set(theme_classic(base_size = 16))
library(dplyr)
 library(tidyquant)
library(lubridate)
library(cowplot)
library(ggiraph)
library(bcrypt)


#Data importeren

df_login <- readRDS("Data/df_login.rds")
df_beheer <- readRDS("Data/df_beheer.rds")
df_lab <- readRDS("Data/df_lab.rds")
df_neerslag <- readRDS("Data/df_neerslag.rds") %>% 
  mutate(Datum = as.POSIXct(Datum))
df_sensor_long <- readRDS("Data/df_sensor_long.rds")
df_sensor <- readRDS("Data/df_sensor.rds")
df_handelingen <- readRDS("Data/df_handelingen.rds")
print("Data loaded")

#Functie om lijst van aanwezige bodemparameters te krijgen
test_nonemptyvar <- function(var) {
  if(!is.numeric(var)) {
    return(FALSE)
  }
  return(sum(var, na.rm = TRUE)>0)
} 

removelist <- c(
  "Datum",
  "monsternummer",
  "Bemonsterde laag",
  "Grondsoort",
  "Hoekpunten (RD)",
  "BOERID",
  "Perceel",
  "Bestand",
  "lab",
  "Gewas"
  )
                
get_parameterlist <- function(x, y) {
  df <- df_lab %>% 
    filter(BOERID %in% x) %>%
    filter(Perceel %in% y) %>%
    select(-contains("unit"), 
           -contains("orig")) %>% 
    select(-removelist) %>% 
    select(where(test_nonemptyvar))
  print(names(df))
  return(names(df))
}

get_perceellist <- function(x) {
  df <- df_lab %>% 
    filter(BOERID %in% x)
  return(unique(df$Perceel))
}

get_sensorperceellist <- function(x) {
  df <- df_beheer %>% 
    filter(BOERID %in% x)
  return(unique(df$Perceel))
}

get_xvar <- function(boer, y) {
  df <- df_lab %>% 
    filter(BOERID %in% boer) %>% 
    filter(!is.na(!!sym(y))) %>% 
    select(-contains("unit"), 
           -contains("orig")) %>% 
    select(where(test_nonemptyvar))
  return(names(df))
}



# ------------------sensordata

# Hier komt nog functie om API te gebruiken
# Idem neerslagdata

# Figures -----------------------------------------------------------------------------------

plot_sensordata <- function(BOERID_sen, 
                            datumrange_perceel,
                            sensorpar
                            ) {
  Interval <- as.duration(
    interval(
      datumrange_perceel[2], 
      datumrange_perceel[1]
      )
    )
 
  df <- df_sensor %>% 
    filter(
      datetime > datumrange_perceel[1],
           datetime < datumrange_perceel[2]
      ) 
    
  u <- "second"
  if (Interval <= 50*60) {u <- "second"}
  else if (Interval > 50*60) {u <- "minute"}
  else if (Interval > 50*60*60) {u <- "hour"}
  else if (Interval > 50*24*60*60) {u <- "day"}
  else {Interval = "month"}
  
  df <- df %>% 
    mutate(datetime = 
             floor_date(
               datetime, 
               unit = u
               )
    )
    
  df_p <- df %>% 
    filter(
      Perceel %in% get_sensorperceellist(BOERID_sen)
      ) %>% 
    filter(!is.na(!!sym(sensorpar)))
  
    
  p <- df_p %>% 
    filter(Perceel %in% get_sensorperceellist(BOERID_sen)) %>% 
    filter(datetime > datumrange_perceel[1],
           datetime < datumrange_perceel[2]) %>% 
    mutate(tp = paste(Perceel, datetime, sep = ", ")) %>% 
    ggplot(aes(datetime, !!sym(sensorpar))) +
    geom_point_interactive(aes(data_id = Perceel,
                              tooltip = tp, 
                              color = Perceel),
                          size = 1, 
                          alpha = 0.1, color = "white") +
    geom_line(aes(color = Perceel)) +
    geom_smooth(data = df, aes(datetime, !!sym(sensorpar))) + 
    xlim(datumrange_perceel[1], datumrange_perceel[2]) +
    xlab("Datum") +
    theme(legend.position = "none")
  
  return(p)
}

  
plot_handeling <- function(BOERID_sen, datumrange_perceel) 
  {
  p <- df_handelingen %>%
    filter(Perceel %in% get_sensorperceellist(BOERID_sen)) %>%
    filter(Datum > datumrange_perceel[1],
           Datum < datumrange_perceel[2]) %>%
    mutate(tp = paste(Perceel, Datum, sep = ", ")) %>% 
    ggplot(aes(Perceel, Datum, color = Perceel, shape = Categorie)) +
    geom_point_interactive(aes(data_id = Handeling, tooltip = tp)) +
    ylim(datumrange_perceel[1], datumrange_perceel[2]) +
    coord_flip() +
    theme(legend.position = "none",
          axis.text = element_blank())
  return(p)
}


plot_neerslag <- function(BOERID_sen,
                          datumrange_perceel) {
  
    df_neerslag %>% 
      filter(boerID  == BOERID_sen) %>% 
      filter(Datum > datumrange_perceel[1],
             Datum < datumrange_perceel[2]) %>% 
      ggplot(aes(Datum, Neerslag)) +
      scale_fill_manual(values = c("regen" = "grey", "sneeuw" = "orange")) +
      geom_col(aes(fill = Soort)) +
      geom_ma(ma_fun = SMA, n = 7, color = "blue") +
      xlim(datumrange_perceel[1], datumrange_perceel[2]) +
      theme(legend.position = c(0.87, 0.87),
            legend.text = element_text(size=10),
            legend.title = element_blank())
}


plot_sensormean <- function(datumrange_perceel, sensorpar) {
  
    df_sensor %>%
      filter(datetime > datumrange_perceel[1],
             datetime < datumrange_perceel[2]) %>% 
      ggplot(aes(datetime, !!sym(sensorpar))) +
      geom_line(aes(group = Perceel), color = "grey") +
      geom_smooth() +
      xlim(datumrange_perceel[1], datumrange_perceel[2])
}


plot_bodemdata <- function(perceel, datumrange, parameter) {
  df_sub <- df_lab %>% 
    filter(
      Datum > datumrange[1],
      Datum < datumrange[2]
      )
  
  df_selection <- df_sub %>% 
    filter(Perceel %in% perceel)
  
  df_sub %>% 
    ggplot(aes(Datum, !!sym(parameter))) +
    geom_line(stat = "smooth", color = "blue", alpha = 0.3) +
    geom_ribbon(stat = "smooth", color = "grey", alpha = 0.1) +
    geom_point_interactive(data = df_selection, aes(data_id = Perceel, tooltip = Perceel)) +
    geom_line(data = df_selection, aes(Datum, !!sym(parameter), color = Perceel)) +
    theme(legend.position = "none")

    
  # df_selection <- df_lab %>% 
  # 
  #   df_lab %>% 
  #     filter(Perceel %in% perceel) %>% 
  #     filter(Datum > datumrange[1],
  #            Datum < datumrange[2]) %>% 
  #     ggplot(aes(Datum, !!sym(parameter), color = Perceel)) +
  #     geom_point_interactive(aes(data_id = Perceel, tooltip = Perceel)) +
  #     geom_line() +
  #     theme(legend.position = "none")

  }


plot_bodemdata_mean <- function(datumrange, parameter) {
  
  df_lab %>%
       filter(Datum > datumrange[1],
              Datum < datumrange[2]) %>%
       ggplot(aes(Datum, !!sym(parameter))) +
       geom_smooth()
  
}

plot_scatter_res <- function(BOERID_res, 
                             datumrange_res,
                             x, 
                             y) {
  
    df_lab %>% 
      filter(BOERID %in% BOERID_res) %>% 
      filter(Datum > datumrange_res[1],
             Datum < datumrange_res[2]) %>% 
      tidyr::unite("id", c(BOERID, Perceel), sep = ": ", remove = FALSE) %>% 
      ggplot(aes(!!sym(x), !!sym(y))) +
       geom_point_interactive(aes(data_id = id, tooltip = id)) 
  
}