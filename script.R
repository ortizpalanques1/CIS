#Libraries and files####

#Libraries
using_packages <- c("tidyverse", "ggplot2")

#Data
lapply(using_packages, require, character.only = TRUE)
unzip(zipfile = "MD3413.zip",
      exdir = "Data")
MD3413_etiq <- read_csv2("Data/3413_etiq.csv")
MD3413_num <- read_csv2("Data/3413_num.csv")


#Vote by party####
a_eliminar <- c("N.C.", "No sabe todavía", "No votaría", "Voto nulo", "En blanco")

vote_by_patry <- MD3413_etiq %>%
  select(`INTENCIONG Intención de voto en las elecciones generales de julio de 2023`) %>% 
  rename("Partido" = `INTENCIONG Intención de voto en las elecciones generales de julio de 2023`) %>% 
  filter(!Partido %in% a_eliminar) %>% 
  group_by(Partido) %>% 
  summarise("Number" = n()) %>% 
  mutate("Percentage" = round(Number/sum(Number)*100,2)) %>% 
  arrange(-Percentage)





