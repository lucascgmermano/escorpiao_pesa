library(glmmTMB)
library(dplyr)
library(broom.mixed)
library(performance)
library(DHARMa)


# Importar dados ----------------------------------------------------------

df <- read.csv("Dados-modelo/df_preliminar.csv",sep = ";",dec = ",",stringsAsFactors = FALSE)

