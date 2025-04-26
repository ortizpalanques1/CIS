# Librerías ####
library(tidyverse)
library(car)
library(writexl)

# Funciones ####
# Estadística desciptiva cuantitative
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

# Estadística descriptiva dicotómica
descriptivos_dicotomicos <- function(variable, valor_0, valor_1){
  valores_dicotomicos <- c(valor_0, valor_1)
  nombre_var <- deparse(substitute(variable))
  nombre_var <- substr(nombre_var, 4, nchar(nombre_var))
  numero_casos <- as.numeric(table(variable))
  porcentaje_casos <- as.numeric(round(prop.table(table(variable))*100,2))
  tomar_valores <- data.frame(
    "Variable" = nombre_var,
    "Valores" = valores_dicotomicos,
    "n" = numero_casos,
    "Porcentaje" = porcentaje_casos
  )
  return(tomar_valores)
}

# Presentacion del modelo logístico
presentation <- function(model_name){
  co <- round(get(model_name)$coefficient,3)
  va <- names(co)
  model_First_table <- data.frame(co, va)
  
  Std_Errors <- data.frame("Std_Errors" = summary(get(model_name))$coefficients[,2])
  Std_Errors$Std_Errors <- round(Std_Errors$Std_Errors, 3)
  Std_Errors$va <- rownames(Std_Errors)
  
  OR <- data.frame("Odds_Ratio" = round(exp(coef(get(model_name))),3))
  OR$va <- rownames(OR)
  
  model_01_Table <- model_First_table %>% 
    left_join(., Std_Errors, by = "va") %>% 
    left_join(., OR, by = "va") %>% 
    select(va, co, Std_Errors, Odds_Ratio)
  
  write_xlsx(model_01_Table, paste0(model_name, ".xlsx"), col_names=TRUE)
}



# Crear data frame ####
MD3413_num <- read_csv2("Data/3413_num.csv")
df <- MD3413_num %>%
  as_tibble() %>% 
  select(SEXO, EDAD, INTENCIONG, NIVELESTENTREV, RELIGION, INGRESHOG) %>% 
  mutate(SEXO = ifelse(SEXO == 1, 1, 0), # Mujer = 0, Hombre = 1
         INTENCIONG = ifelse(INTENCIONG == 1, 1, 0), # PSOE = 1, Los demás = 0
         NIVELESTENTREV = ifelse(NIVELESTENTREV %in% c(16, 98, 0), NA, NIVELESTENTREV),
         RELIGION =  dplyr::recode(RELIGION,
                            `1` = 1,
                            `2` = 1,
                            `0` = 99,
                            .default = 0
         ),
         RELIGION = ifelse(RELIGION == 99, NA, RELIGION),
         INGRESHOG = dplyr::recode(INGRESHOG,
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

variables_independientes <- c("SEXO", "EDAD", "RELIGION", "NIVELESTENTREV", "INGRESHOG") 

# Número de casos
n_total <- nrow(df)
# Variables cuantitativas
ed_edad <- descriptivos(df$EDAD)
ed_nivel_edu <- descriptivos(df$NIVELESTENTREV)
ed_ingreso <- descriptivos(df$INGRESHOG)
nombre_var_cuantitativa <- colnames(df)[c(2,4,6)]

write_csv(
  bind_rows(
    ed_edad,
    ed_nivel_edu, 
    ed_ingreso
  ), 
  "Tablas/tabla_cuantitativa.csv")

# Variables dicotómicas
ed_sexo <- descriptivos_dicotomicos(df$SEXO, "Mujer", "Hombre")
ed_religion <- descriptivos_dicotomicos(df$RELIGION, "No catolico", "Catolico")

write_csv(
  bind_rows(
    ed_sexo,
    ed_religion
  ),
  "Tablas/tabla_dicotomica.csv"
)

# Revisar para colinearidad ####
model <- lm(INTENCIONG ~ SEXO + EDAD + RELIGION + NIVELESTENTREV + INGRESHOG, data = df)
vif_resultados <- vif(model)

write_csv(
  data.frame(
    "Variable" = names(vif_resultados),
    "VIF" = unname(vif_resultados)
  ),
  "Tablas/tabla_vif.csv"
)

# Regresión logística ####
# Modelo con todas las variables
model_01 <- glm(INTENCIONG ~ SEXO+EDAD+RELIGION+NIVELESTENTREV+INGRESHOG+
                  SEXO*EDAD+
                  SEXO*RELIGION+
                  SEXO*NIVELESTENTREV+
                  SEXO*INGRESHOG+
                  EDAD*RELIGION+
                  EDAD*NIVELESTENTREV+
                  EDAD*INGRESHOG+
                  RELIGION*NIVELESTENTREV+
                  RELIGION*INGRESHOG+
                  NIVELESTENTREV*INGRESHOG,
                data = df, family = binomial(logit))

# Resumen
summary(model_01)

presentation("model_01")


# Model 01 Odds Ratio #
exp(coef(model_01))

# Model 01 Confidence intervals #
exp(cbind(coef(model_01), confint(model_01)))  


# Modelo 2
model_02 <- glm(INTENCIONG ~ SEXO+EDAD+NIVELESTENTREV+
                  SEXO*EDAD+
                  SEXO*NIVELESTENTREV+
                  EDAD*NIVELESTENTREV,
                data = df, family = binomial(logit))

# Resumen
summary(model_02)

presentation("model_02")


# Model 02 Odds Ratio #
exp(coef(model_02))

# Model 02 Confidence intervals #
exp(cbind(coef(model_02), confint(model_02)))  

# Modelo 3
model_03 <- glm(INTENCIONG ~ SEXO+EDAD+NIVELESTENTREV+
                  SEXO*EDAD+
                  SEXO*NIVELESTENTREV,
                data = df, family = binomial(logit))

# Resumen
summary(model_03)

presentation("model_03")


# Model 03 Odds Ratio #
exp(coef(model_03))

# Model 03 Confidence intervals #
exp(cbind(coef(model_03), confint(model_03)))  
