#Laden der Libraries
library(readr)
library(lubridate)
library(ggplot2)
library(dplyr)

#Daten laden und warengruppen extrahieren
train_data <- read.csv("0_DataPreparation/joinedData.csv")
validation_data

# Erstellen eines linearen Models
mod <- lm(Umsatz ~ as.factor(Warengruppe)+Temperatur+KielerWoche, ergebnis_tibble)
summary(mod)

# Nutzung des trainierten Modells für eine Vorhersage
predicted_Values <- predict(mod, newdata = validation_data)

comparison <- data.frame(Actual = train_data$umsatz, Predicted = predicted_Values)

rmse <- sqrt(mean((comparison$Actual - comparison$Predicted)^2))

head(comparison)
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
# Ziel: Maximieren des RMSE
