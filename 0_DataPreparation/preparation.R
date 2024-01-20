# drei csv zusammenführen als tibble
# Funktionen aus skimR und DataExplorer nutzen, um Daten in Tibble genauer anzusehen

#Installieren von Packages falls, diese noch nicht installiert sind
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(skimr)) install.packages("skimr")
if (!require(DataExplorer)) install.packages("DataExplorer")
if (!require(dplyr)) install.packages("dplyr")
if (!require(lubridate)) install.packages("lubridate")
if (!require(readr)) install.packages("readr")
if (!require(VIM)) install.packages("VIM")
if (!require(colorspace)) install.packages("colorspace")
if (!require(grid)) install.packages("grid")
if (!require(zoo)) install.packages("zoo")

# Laden der Libraries
library(tidyverse)
library(skimr)
library(DataExplorer)
library(dplyr)
library(lubridate)
library(readr)
library(VIM)
library(colorspace)
library(grid)
library(zoo)

# Einlesen der Daten
kiwo <- read.csv("0_DataPreparation/kiwo.csv")
wetter <- read.csv("0_DataPreparation/wetter.csv")
umsatz_train <- read.csv("0_DataPreparation/train.csv")
umsatz_test <- read.csv("0_DataPreparation/test.csv")
feiertage <- read.csv("0_DataPreparation/Feiertage.csv", sep = ";")
schulferien <- read.csv("0_DataPreparation/Schulferien.csv")

# Sortieren der Datumsangaben nach Jahr, Monat und Tag
umsatz_train$Datum <- ymd(umsatz_train$Datum)
umsatz_test$Datum <- ymd(umsatz_test$Datum)
wetter$Datum <- ymd(wetter$Datum)
kiwo$Datum <- ymd(kiwo$Datum)

# eigene Variablen
schulferien$Datum <- ymd(schulferien$Datum)

# die Feiertage als Datum
feiertage$Datum <- as.Date(feiertage$Datum, format = "%d.%m.%Y")


# Zusammenführen der Daten als Tibble und speichern in einer CSV-Datei
merged_data_train <- umsatz_train %>%
  left_join(wetter, by = "Datum") %>%
  left_join(kiwo, by = "Datum") %>%
  left_join(feiertage, by = "Datum") %>%
  left_join(schulferien, by = "Datum")

ergebnis_tibble_train <- as_tibble(merged_data_train)
write.csv(ergebnis_tibble_train, file="0_DataPreparation/joinedData_train.csv")

# Testdaten als Tibble zusammenfassen und speichern
merged_data_test <- umsatz_test %>%
  left_join(wetter, by = "Datum") %>%
  left_join(kiwo, by = "Datum") %>%
  left_join(feiertage, by = "Datum") %>%
  left_join(schulferien, by = "Datum")

ergebnis_tibble_test <- as_tibble(merged_data_test)
write.csv(ergebnis_tibble_test, file="0_DataPreparation/joinedData_test.csv")

# Daten bereinigen und NaN ersetzen
train_data <- read.csv("0_DataPreparation/joinedData_train.csv")
plot <- aggr(train_data, combined=TRUE, numbers=TRUE)
print(plot)

for (spalte in names(train_data)){
  hat_nan <- any(is.na(train_data[[spalte]]))
  print(spalte)
  print(hat_nan)
}

train_data$KielerWoche <- ifelse(is.na(train_data$KielerWoche), 0, 1)
train_data$Feiertag <- ifelse(is.na(train_data$Feiertag), 0, 1)
train_data$Ferien <- ifelse(is.na(train_data$Ferien), 0, 1)

train_data$Wettercode <- na.locf(train_data$Wettercode, fromLast = TRUE, na.rm = FALSE)
print(train_data)

# Ergebnis speichern
