#Laden der Libraries
library(readr)
library(lubridate)
library(ggplot2)
library(dplyr)

#Daten laden und warengruppen extrahieren
umsatzdaten <- read.csv("umsatzdaten_gekuerzt.csv")
warengruppen <- unique(umsatzdaten$Warengruppe)

#Spalte Tage hinzufügen
umsatzdaten$Datum <- as.Date(umsatzdaten$Datum)
umsatzdaten$tage <- factor(weekdays(umsatzdaten$Datum), levels=c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag"))

# Umsatz pro Tag für alle Warengruppen
gesamtumsatz_pro_tag <- umsatzdaten %>%
  group_by(tage) %>%
  summarize(Gesamtumsatz = sum(Umsatz),
            n=n(), 
            mean=mean(Umsatz),
            sd=sd(Umsatz)) %>%
  mutate( se=sd/sqrt(n)) %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

print(gesamtumsatz_pro_tag)


balkendiagrammeinfach <- ggplot(gesamtumsatz_pro_tag, aes(x = tage, y = mean)) +
  geom_bar(stat = "identity", position = "dodge", fill="#800080")+
  geom_errorbar( aes(x=tage, ymin=mean-ic, ymax=mean+ic), width=0.4, colour="orange", alpha=0.9, size=1.5)+
  labs(title = "Gesamtumsatz pro Tag für alle Warengruppen mit Konfidenzintervallen", x = "Wochentag", 
       y = "Gesamtumsatz")

print(balkendiagrammeinfach)

#Umsatz pro Tag je Warengruppe
gesamtumsatz_pro_tag_warengruppe <- umsatzdaten %>%
  group_by(Warengruppe, tage) %>%
  summarize(Gesamtumsatz = sum(Umsatz))

print(gesamtumsatz_pro_tag_warengruppe)

balkendiagramm <- ggplot(gesamtumsatz_pro_tag_warengruppe, aes(x = Warengruppe, y = Gesamtumsatz, fill = tage)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Gesamtumsatz pro Tag und Warengruppe", x = "Warengruppe", y = "Gesamtumsatz", fill="Wochentag") +
  scale_fill_brewer(palette = "Set3")


# Diagramm anzeigen
print(balkendiagramm)

