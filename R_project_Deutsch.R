### 25 Leistungsnachweis
##### Muhammed SEVIM

#######################################

### 25.1 Kleine Auswertungen mit dem European Social Survey

## 1. Wir laden und lesen die Daten

load(url("http://www.farys.org/daten/ESS.RDATA"))
ess <- read.table("http://www.farys.org/daten/ESS.RDATA", header=TRUE)
ess

## 2.
# Wir k�nnen die Anzahl der L�nder mit der Funktion "group_by" ermitteln. 
# Wir k�nnen mit der Funktion "summarise" auch sehen, wie oft sich diese L�nder in den Daten wiederholen.
library(dplyr)
library(tidyr)
ess1 <- ess %>%
  group_by(Land) %>%
  summarise(n=n())%>%
  arrange((n))
ess1
# Die Befragten im Datensatzaus stammen aus 29 L�ndern.
# Auch dies k�nnen wir mit einem �berblick �ber die Daten verstehen.
str(ess)
glimpse(ess$Land)
summary(ess$Land)
head(ess$Land)
tail(ess$Land)

## 3.
essranking <- ess %>%
  group_by(Land) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
essranking
# Laut Ranking hat Deutschland mit "20490" die meisten Befragten.

## 4.
# Welches ist das durchschnittlich gl�cklichste Land im Datensatz? 
# Um den Mittelwert zu berechnen, m�ssen wir die "NA"s in der "happy" Variable ausschlie�en.
esshappy <- ess %>%
  group_by(Land) %>%
  summarise(gluecklichste = mean(happy, na.rm=TRUE))%>%
  arrange(-gluecklichste)
esshappy
# Die gl�cklichsten L�nder mit durchschnittlich ca."8.32" sind D�nemark und Island.
# D�nemark = 8.318701; und Island = 8.317557

# Welches das ungl�cklichste?
# Wenn wir die Liste umkehren, erhalten wir das Ergebnis.
essunhappy <- ess %>%
  group_by(Land) %>%
  summarise(ungluecklichste = mean(happy, na.rm=TRUE))%>%
  arrange(ungluecklichste)
essunhappy
# Das ungl�cklichste Land mit durchschnittlich ca."5.30" ist Bulgarien.
# Bulgarien = 5.300183

## 5.
# Wir organisieren die Daten nach dem Zufriedenheitswert = 10.
ess_vollkommen <- ess %>% 
  select(Land, happy) %>%
  filter(happy==10) %>%
  group_by(Land,happy) %>%
  summarise(count=n())%>%
  arrange(desc(count))
ess_vollkommen
# In D�nemark gibt es am meisten "vollkommen gl�ckliche Menschen"
# mit "2108" angegebener Zufriedenheitswert (10).

## 6.
# Zun�chst ermitteln wir die Durchschnittsgl�ck �ber die Jahre 2008 und 2010.
# Zufriedenheitswerte f�r 2008 und 2010 sind m�glicherweise nicht f�r alle L�nder verf�gbar.
year2008 <- ess %>%
  filter(year==2008) %>%
  group_by(Land) %>%
  summarise(mean2008 = mean(happy, na.rm=TRUE))
year2008

year2010 <- ess %>%
  filter(year==2010) %>%
  group_by(Land) %>%
  summarise(mean2010 = mean(happy, na.rm=TRUE))
year2010
# Wir k�nnen diese beiden Daten �ber die gemeinsame "Land"-variable kombinieren.
# Wir k�nnen "merge" oder "join" Funktionen verwenden.
# Mit full_join k�nnen wir auch die L�nder sehen, 
# die f�r 2008 und 2010 keinen Zufriedenheitswert(happy) haben.
average <- full_join(year2008, year2010, by="Land") %>%
  mutate(mean.drop = mean2008-mean2010) %>%
  arrange(-mean.drop)
average

# Irland hatte von 2008 bis 2010 den gr�ssten R�ckgang im Durchschnittsgl�ck.
# "mean.drop" Variable zeigt die Ver�nderung zwischen den Jahren.
# Der Unterschied f�r Irland: 7.548186 - 6.848249 = 0.69993691
# F�r 2010 liegen keine Daten f�r die T�rkei vor.
# F�r 2008 liegen keine Daten f�r die Litauen vor.

#######################################

### 25.2 ggplot Bastelei

install.packages("quantmod")
library(quantmod)
getSymbols("LISN.SW",from ="2007-01-01")
df <- data.frame(time(LISN.SW), LISN.SW[,6])

# Ich benenne die Variablen entsprechend.
names(df) <- c("zeit", "entwicklung")

# Um die zeitliche �nderung zu sehen, bilde ich eine neue "kursent" Variable.
df1 <- df %>%
  mutate(ent = entwicklung - first(entwicklung),
         kursent = ent/first(entwicklung)+1) 

# Ich erstelle die Grafik.
# Gleichzeitig exportiere ich die Grafik als PDF.
library(ggplot2) 
library(scales)

pdf("Kursentwicklung.pdf")
ggplot(df1, aes(x = zeit, y=kursent))+
  geom_ribbon(aes(ymin=pmin(kursent,1), ymax=1), fill="red3", col="red3", alpha=0.4) +
  geom_ribbon(aes(ymin=1, ymax=pmax(kursent,1)), fill="green3", col="green3", alpha=0.4) +
  geom_hline(aes(yintercept=1), color="black") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
  scale_y_continuous(breaks = seq(.5, 3.5, by = .5), 
                     labels = paste0(seq(50, 350, by = 50), "%"))+
  theme_bw()+
  labs(title = 'Lindt & Spr�ngli Kursentwicklung seit Januar 2007',
       y = NULL,      
       x = 'Jahr')
dev.off() 
  
#######################################

### 25.3 Webscraping / Tidying

## 1. Wir lesen die Tabelle
library(rvest)
klima <- read_html("https://de.wikipedia.org/wiki/Bern#Klima")
table <- html_table(klima, fill = TRUE, header = TRUE)[[5]]
table
# [[5]]ist unsere Tabelle auf Website. 

## 2. 
# Wir konzentrien uns auf die ersten drei Zeilen.
# ("Monat", "Max. Temperatur", "Min. Temperatur")
library(tidyr)
library(stringr)
#ersten 3 Zeilen der Tabelle und Monate
t1 <- table[1:2, 1:13]
t1

# Daten (Monate) reshapen von wide nach long
colnames(t1)[1] <- "wetter"
t2 <- t1 %>% 
  pivot_longer(!wetter, names_to = c("Monat"), values_drop_na = TRUE)
t2
# Min-Max Temparature reshapen
t3 <- pivot_wider(t2, names_from = wetter)
t3

# um die 2. und 3. Spalte umzubenennen...
colnames(t3)[colnames(t3) %in% 
       c("Max. Temperatur (�C)", "Min. Temperatur (�C)")] <- c("Max", "Min")               

# Monatsnamen korrigieren
t3$Monat <- paste0(t3$Monat, c("uar", "ruar", "z", "il", 
                               "","i", "i", "ust",
                            "tember", "ober", rep("ember", 2)))
t3

# Wenn Kommas durch Punkte ersetzt werden (","->"."), erscheinen "NA"s f�r negative Zahlen. 
# Ich habe einige alternative M�glichkeiten zur Behebung des Problems ausprobiert, 
# konnte dieses Problem jedoch nicht beheben.
# Also musste ich negative Zahlen erneut eingeben.
t3[ , 2:3] <- lapply(t3[ , 2:3],
                     function(x){as.numeric(as.character(gsub(",", ".", x))) 
                       })
t3
# Oder
#t3$Max <- as.numeric(gsub("\\,", ".", t3$Max))
#t3$Min <- as.numeric(gsub("\\,", ".", t3$Min))
#t3

# negative Zahlen als NA
t3$Min[is.na(t3$Min)] <- c(-3.6, -3.1, -2.3)
t3

## Wir h�bschen and exportieren die Tabelle als HTML
install.packages("huxtable")
library(huxtable)
library(dplyr)
# Ich l�se sie aufgrund des Bildes der Tabelle in der Frage 
# mit zwei Alternativen.
# a) Wenn die Zahlen (1-12) und "> tabelle" die �berschrift (caption)
# nicht Teil der Tabelle sind, ist hier die L�sung:
t3 <- as_hux(t3) %>%
    map_align(., by_cols( "right", "right", "right"))%>%
    set_position("left")%>%
    set_number_format(1)
t3

# Wenn wir die Farbe der Zahlen �ndern m�chten...
#t3 <- as_hux(t3) %>%
#    map_align(., by_cols( "right", "right", "right"))%>%
#    set_position("left")%>%
#    set_text_color(2:13, 2:3, "lightseagreen")%>%
#    set_number_format(1)
#t3

## 3. Jetz k�nnen wir die Tabelle als HTML exportieren...
quick_html(t3, file ="Klima_Bern.html")

# b) Wenn die Zahlen (1-12) auf der linken Seite 
# und "> tabelle" als die �berschrift (caption) Teil der Tabelle sind, 
# w�re die L�sung:
# Wir brauchen eine neue Spalte:
reihe <- c("", c(1:12))

# Tabelle mit neuer Spalte:
t3 <- as_hux(t3) %>%
    insert_column(reihe, after = 0)%>%
    map_align(., by_cols("left", "right", "right", "right"))%>%
    set_text_color(2:13, -2, "lightseagreen")%>%
    set_col_width(2:4, 0)%>%
    set_position("left")%>%
    set_caption("> tabelle")%>% 
    set_caption_pos(., "topleft")
t3

## 3. Die Tabelle als HTML:
quick_html(t3, file ="Klima_Bern_1.html")





