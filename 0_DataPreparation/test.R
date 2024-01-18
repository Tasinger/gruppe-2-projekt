# drei csv zusammenführen als tibble
# Funktionen aus skimR und DataExplorer nutzen, um Daten in Tibble genauer anzusehen

library(tidyverse)
library(skimR)
library(DataExplorer)
library(dplyr)
library(lubridate)

kiwo <- read.csv("0_DataPreparation/kiwo.csv")
wetter <- read.csv("0_DataPreparation/wetter.csv")
umsatz <- read.csv("0_DataPreparation/umsatzdaten_gekuerzt.csv")

umsatz$Datum <- ymd(umsatz$Datum)
wetter$Datum <- ymd(wetter$Datum)
kiwo$Datum <- ymd(kiwo$Datum)

data <- left_join(umsatz,kiwo,by = "Datum")
ergebnis <- left_join(data,wetter, by= "Datum")

ergebnis_tibble <- as_tibble(ergebnis)
write.csv(ergebnis_tibble, file="0_DataPreparation/joinedData.csv")

mod <- lm(Umsatz ~ as.factor(Warengruppe)+Temperatur+KielerWoche, ergebnis_tibble)
summary(mod)
