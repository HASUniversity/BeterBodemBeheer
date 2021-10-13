library(readxl)
library(tidyverse)
theme_set(theme_classic(base_size = 16))
library(tidyquant)
library(lubridate)
library(plotly)
# library(ggpubr)
# library(gridExtra)
# library(grid)
library(cowplot)
library(ggiraph)
library(bcrypt)

#Inloggegevens importeren
df_login <- read_excel("Data/up.xlsx")
# status <- 0

#Data importeren

df_lab <- read_excel("Data/bodemdata_totaal_v2.xlsx", 
                     sheet = "Data") %>%
  mutate(Datum  = lubridate::date(Datum))
df_beheer <- read_excel("Data/beheer.xlsx") %>% 
  mutate(SensorID = as.character(SensorID))
df_sensor <- left_join(read_excel("Data/sensordata.xlsx") %>% 
                         mutate(SensorID = as.character(SensorID)) %>% 
                         filter(!is.na(datetime)), 
                       df_beheer %>% select(SensorID, Perceel))



#Functie om lijst van aanwezige bodemparameters te krijgen
test_nonemptyvar <- function(var) {
  if(!is.numeric(var)) {
    return(FALSE)
  }
  return(sum(var, na.rm = TRUE)>0)
} 

get_parameterlist <- function(x) {
  df <- df_lab %>% 
    filter(BOERID %in% x) %>% 
    select(-contains("unit"), 
           -contains("orig")) %>% 
    select(where(test_nonemptyvar))
  return(names(df))
}

get_perceellist <- function(x) {
  df <- df_lab %>% 
    filter(BOERID == x)
  return(unique(df$Perceel))
}

get_sensorperceellist <- function(x) {
  df <- df_beheer %>% 
    filter(BOERID == x)
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

# Gebruiksdata

# Functie om iedere sheet uit te lezen
# NB: voorlopig op datumniveau wegfilteren foute data
get_intervals <- function(datum, status) {
  
  df <- tibble(datum, status=status) %>% filter(!is.na(status))
  d <- df$datum
  s <- df$status
  
  oke <- d[s == "ok"]
  stuk <- d[s == "stuk"]
  
  if(length(oke)>length(stuk)) {
    stuk <- append(stuk, today())
  }
  
  return(map2(oke+1, stuk, interval))
}


read_sensorstatus <- function(p, s) {
  
  df <- read_excel(p, sheet = s) %>% 
    mutate(datum = date(datum)) %>% 
    mutate(sensorID = s)
  return(df)
  
}

p_sensorstatus <- "Data/sensorstatus.xlsx"
sheets <- tail(excel_sheets(p_sensorstatus),-1)

df_sensor_long <- df_sensor %>% 
  pivot_longer(c(EC,Temp, Bodemvocht, O2, pH),
               names_to = "sensor",
               values_to = "waarde") %>% 
  mutate(status = "stuk")


for(id in sheets) {
  df <- read_sensorstatus(p_sensorstatus, id) 
  for(s in c('pH', "O2", "EC", "Temp", "Bodemvocht")) {
    intervals <- get_intervals(df$datum, df[s] %>% pull(1))
    for(i in intervals) {
      
      df_sensor_long$status[df_sensor_long$SensorID == id &
                              df_sensor_long$Datum %within% i &
                              df_sensor_long$sensor == s] <- "ok"
    }
  }
}
df_sensor <- df_sensor_long %>%
  filter(status == "ok") %>% 
  pivot_wider(names_from = sensor,
              values_from = waarde,
              values_fill = NA)


# --- Neerslagdata
# Voorlopig op basis van Excel, later mogelijk API


read_neerslag <- function(p, s) {
  df <- read_excel(p, sheet = s) %>%
    mutate(Datum = ymd(Datum)) %>% 
    mutate(Soort = ifelse(Sneeuw>0, "sneeuw", "regen")) %>% 
    mutate(boerID = s)
  
}

p <- "Data/Neerslag data.xlsx"
sheets <- excel_sheets(p)
df_neerslag <- map2_dfr(p, sheets, read_neerslag)

read_handelingen <- function(p, sheet) {
  df <- read_excel(p, sheet) %>% 
    mutate(SensorID = sheet,
           Datum = as_datetime(Datum),
           Handeling =  as.character(Handeling),
           Categorie = as.character(Categorie)) 
  df <- left_join(df,df_beheer %>% select(SensorID, Perceel))
  return(df)
}

p_handelingen <- "Data/Teelthandelingen sensoren.xlsx"
sheets <- excel_sheets(p_handelingen)
df_handelingen <- map2_dfr(p_handelingen, sheets, read_handelingen)


# Figures -----------------------------------------------------------------------------------

plot_sensordata <- function(BOERID_sen, 
                            datumrange_perceel,
                            sensorpar
                            ) {
  p <- df_sensor %>% 
    filter(Perceel %in% get_sensorperceellist(BOERID_sen)) %>% 
    filter(datetime > datumrange_perceel[1],
           datetime < datumrange_perceel[2]) %>% 
    ggplot(aes(datetime, !!sym(sensorpar), color = Perceel)) +
    geom_line_interactive(aes(data_id = Perceel,
                              tooltip = Perceel),
                          size = 1, 
                          alpha = 0.8) +
    # xlim(datumrange_perceel[1], datumrange_perceel[2]) +
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
    ggplot(aes(Perceel, Datum, color = Perceel, shape = Categorie)) +
    geom_point_interactive(aes(data_id = Handeling, tooltip = Handeling)) +
    coord_flip() +
    # ylim(datumrange_perceel[1], datumrange_perceel[2]) +
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
      geom_smooth()
}


plot_bodemdata <- function(perceel, datumrange, parameter) {
  
    df_lab %>% 
      filter(Perceel %in% perceel) %>% 
      filter(Datum > datumrange[1],
             Datum < datumrange[2]) %>% 
      ggplot(aes(Datum, !!sym(parameter), color = Perceel)) +
      geom_point_interactive(aes(data_id = Perceel, tooltip = Perceel)) +
      geom_line() +
      theme(legend.position = "none")

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
      unite("id", c(BOERID, Perceel), sep = ": ", remove = FALSE) %>% 
      ggplot(aes(!!sym(x), !!sym(y))) +
      #geom_point(aes(color = BOERID), size = 3) 
       geom_point_interactive(aes(data_id = id, tooltip = id)) 
     # geom_smooth(method = "lm")
  
}