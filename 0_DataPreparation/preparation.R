# drei csv zusammenführen als tibble
# Funktionen aus skimR und DataExplorer nutzen, um Daten in Tibble genauer anzusehen

# Laden der Libraries
library(tidyverse)
library(skimr)
library(DataExplorer)
library(dplyr)
library(lubridate)
library(readr)

# Einlesen der Daten
kiwo <- read.csv("0_DataPreparation/kiwo.csv")
wetter <- read.csv("0_DataPreparation/wetter.csv")
umsatz <- read.csv("0_DataPreparation/train.csv")
feiertage <- read.csv("0_DataPreparation/Feiertage.csv")
schulferien <- read.csv("0_DataPreparation/Schulferien.csv")

# Sortieren der Datumsangaben nach Jahr, Monat und Tag
umsatz$Datum <- ymd(umsatz$Datum)
wetter$Datum <- ymd(wetter$Datum)
kiwo$Datum <- ymd(kiwo$Datum)

# Sortieren der eigene Variablen nach Jahr, Monat, Tag
feiertage$Datum <- ymd(feiertage$Datum)
schulferien$Datum <- ymd(schulferien$Datum)

#Zusammenführen der Daten als Tibble und speichern in einer CSV-Datei
merged_data <- umsatz %>%
  left_join(wetter, by = "Datum") %>%
  left_join(kiwo, by = "Datum") %>%
  left_join(feiertage, by = "Datum") %>%
  left_join(schulferien, by = "Datum")

ergebnis_tibble <- as_tibble(merged_data)
write.csv(ergebnis_tibble, file="0_DataPreparation/joinedData.csv")

#Erstellen eines linearen Models
mod <- lm(Umsatz ~ as.factor(Warengruppe)+Temperatur+KielerWoche, ergebnis_tibble)
summary(mod)


