# drei csv zusammenführen als tibble
# Funktionen aus skimR und DataExplorer nutzen, um Daten in Tibble genauer anzusehen

library(tidyverse)
library(skimr)
library(DataExplorer)
library(dplyr)
library(lubridate)
library(readr)


kiwo <- read.csv("0_DataPreparation/kiwo.csv")
wetter <- read.csv("0_DataPreparation/wetter.csv")
umsatz <- read.csv("0_DataPreparation/train.csv")
feiertage <- read.csv("0_DataPreparation/Feiertage.csv", sep = ";")
schulferien <- read.csv("0_DataPreparation/Schulferien.csv")

umsatz$Datum <- ymd(umsatz$Datum)
wetter$Datum <- ymd(wetter$Datum)
kiwo$Datum <- ymd(kiwo$Datum)

# eigene Variablen
schulferien$Datum <- ymd(schulferien$Datum)

# die Feiertage sind als dd.mm.yyyy gespeichert...
feiertage$Datum <- as.Date(feiertage$Datum, format = "%d.%m.%Y")
# sollen aber als mm-dd-yyyy gespeichert werden
feiertage$Datum_neues_format <- format(feiertage$Datum, "%m-%d-%Y")

merged_data <- umsatz %>%
  left_join(wetter, by = "Datum") %>%
  left_join(kiwo, by = "Datum") %>%
  left_join(feiertage, by = "Datum") %>%
  left_join(schulferien, by = "Datum")

ergebnis_tibble <- as_tibble(merged_data)
write.csv(ergebnis_tibble, file="0_DataPreparation/joinedData.csv")

mod <- lm(Umsatz ~ as.factor(Warengruppe)+Temperatur+KielerWoche, ergebnis_tibble)
summary(mod)

