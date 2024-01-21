# Laden der Libraries
library(readr)
library(lubridate)
library(ggplot2)
library(dplyr)

# Daten laden und warengruppen extrahieren
data <- read.csv("0_DataPreparation/joinedData.csv")

# Calculate the number of rows for each dataset
n_total <- nrow(data)
n_train <- floor(0.7 * n_total)
n_validation <- floor(0.20 * n_total)

# Teile die Daten in Trainings-, Validierungs- und Testdaten auf
train_data <- data %>% slice(1:n_train)
validation_data <- data %>% slice((n_train + 1):(n_train + n_validation))
test_data <- data %>% slice((n_train + n_validation + 1):n_total)

# TODO in der as.factor werden die Warengruppen anscheinend unterschiedlich betrachtet?
# ChatGPT zitat: "Der Fehler "Faktor 'as.factor(Warengruppe)' hat neue Stufen 6" deutet darauf hin,
# dass es in den Validierungsdaten neue Werte in der Variable Warengruppe gibt,
# die im Trainingsdatensatz nicht vorhanden waren."

# Erstellen eines linearen Models
mod <- lm(Umsatz ~ as.factor(Warengruppe) + Temperatur + KielerWoche, train_data)
summary(mod)

# Nutzung des trainierten Modells für eine Vorhersage
predicted_Values <- predict(mod, newdata = validation_data)

comparison <- data.frame(Actual = validation_data$umsatz, Predicted = predicted_Values)

rmse <- sqrt(mean((comparison$Actual - comparison$Predicted)^2))

head(comparison)
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
# Ziel: Maximieren des RMSE


