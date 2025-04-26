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

# Estadística descriptiva ####
# Número de casos
n_total <- nrow(df)
# Variables cuantitativas
descriptivos <- function(variable){
  nombre_var <- deparse(substitute(variable))
  nombre_var <- substr(nombre_var, 4, nchar(nombre_var))
  minimo <- min(variable)
  maximo <- max(variable)
  media <- mean(variable)
  des_est <- sd(variable)
  tomar_valores <- data.frame(
    "Variable" = nombre_var,
    "M" = round(media, 2),
    "SD" = round(des_est, 2),
    "Min" = minimo,
    "Max" = maximo
  )
  return(tomar_valores)
}
ed_edad <- descriptivos(df$EDAD)
ed_nivel_edu <- descriptivos(df$NIVELESTENTREV)
ed_ingreso <- descriptivos(df$INGRESHOG)
nombre_var_cuantitativa <- colnames(df)[c(2,4,6)]

tabla_cuantitativa <- write_csv(bind_rows(
  ed_edad, 
  ed_nivel_edu, 
  ed_ingreso)
  , "tabla_cuantitativa.csv")


# Revisar para autocorrelación ####

# Regresión logística ####

