# Modelos_Multinominales
# ==========================================================
# Universidad de Guanajuato
# Sergio Arturo Flores Segoviano
# Tema: Modelos categóricos multinomiales
# Ejemplo adaptado: Países (World Bank 50)
# https://github.com/SergioSegoviano/Modelos_Multinominales.git
# ==========================================================

# (1) Instalar y cargar librerías
# ----------------------------------------------------------
install.packages("nnet")
install.packages("marginaleffects")
install.packages("readxl")
install.packages("dplyr")

library(nnet)
library(marginaleffects)
library(readxl)
library(dplyr)
View(df)


# (2) Cargar base de datos
# ----------------------------------------------------------
df <- read_excel("worldbank_50.xlsx")

# Revisar estructura
str(df)
head(df)

# (3) Definir variable dependiente y ajustar modelo
# ----------------------------------------------------------
# Variable dependiente: GDP_per_capita (Bajo, Mediano, Alto)
# Variables independientes: Poverty, Literacy, Life_expectancy

# Aseguramos que la variable dependiente sea factor
df$GDP_per_capita <- factor(df$GDP_per_capita, 
                            levels = c("Bajo","Mediano","Alto"))

# Ajustar modelo multinomial
mod_wb <- multinom(GDP_per_capita ~ Poverty + Literacy + Life_expectancy, 
                   data = df, trace = FALSE)

summary(mod_wb)

# (4) Interpretación de coeficientes
# ----------------------------------------------------------
# Odds ratios
exp(coef(mod_wb))

# (5) Efectos marginales promedio (AME)
# ----------------------------------------------------------
ame_poverty <- avg_slopes(mod_wb, variables = "Poverty", type = "probs")
ame_literacy <- avg_slopes(mod_wb, variables = "Literacy", type = "probs")
ame_life <- avg_slopes(mod_wb, variables = "Life_expectancy", type = "probs")

ame_poverty
ame_literacy
ame_life
