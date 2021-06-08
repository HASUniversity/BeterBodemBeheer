library(readxl)
library(tidyverse)
theme_set(theme_classic(base_size = 16))
library(lubridate)


#Data importeren
df_lab <- read_excel("bodemdata_totaal_v2.xlsx",
                     sheet = "Data") %>% 
  mutate(Datum  = lubridate::date(Datum))
df_beheer <- read_excel("beheer.xlsx") %>% 
  mutate(SensorID = as.character(SensorID))
df_sensor <- left_join(read_excel("sensordata.xlsx") %>% 
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

p_sensorstatus <- "sensorstatus.xlsx"
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

p <- "Neerslag data.xlsx"
sheets <- excel_sheets(p)
df_neerslag <- map2_dfr(p, sheets, read_neerslag)
