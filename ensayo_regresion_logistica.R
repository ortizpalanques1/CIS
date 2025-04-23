# Librerías ####
library(tidyverse)

# Crear data frame ####
df <- MD3413_num %>%
  as_tibble() %>% 
  select(SEXO, EDAD, INTENCIONG, NIVELESTENTREV, RELIGION, INGRESHOG) %>% 
  mutate(SEXO = ifelse(SEXO == 1, 1, 0), # Mujer = 0, Hombre = 1
         INTENCIONG = ifelse(INTENCIONG == 1, 1, 0), # PSOE = 1, Los demás = 0
         NIVELESTENTREV = ifelse(NIVELESTENTREV %in% c(16, 98, 0), NA, NIVELESTENTREV),
         RELIGION =  recode(RELIGION,
                            `1` = 1,
                            `2` = 1,
                            `0` = 99,
                            .default = 0
         ),
         RELIGION = ifelse(RELIGION == 99, NA, RELIGION),
         INGRESHOG = recode(INGRESHOG,
                            `1` = 6,
                            `2` = 5,
                            `3` = 4,
                            `4` = 3,
                            `5` = 2,
                            `6` = 1,
                            .default = 0
         ),
         INGRESHOG = ifelse(INGRESHOG == 0, NA, INGRESHOG)) %>% 
  drop_na()

# Revisar para autocorrelación ####

# Regresión logística ####

