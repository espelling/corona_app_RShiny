library(shiny)
library(ggplot2)

##Data preparation
##read table -> download the file from https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0
tabelle <- read.csv("path of your csv file", encoding = "UTF-8")

##Number of cases per reporting date
FallProDatum <- aggregate(AnzahlFall~Meldedatum, data = tabelle, FUN = sum)

##Number of deaths per reporting date
TodeProDatum <- aggregate(AnzahlTodesfall~Meldedatum, data = tabelle, FUN = sum)

##Remove timestamp from date
FallProDatum$Meldedatum <- as.Date(FallProDatum$Meldedatum)
TodeProDatum$Meldedatum <- as.Date(TodeProDatum$Meldedatum)

##merge both tables into one
Gesamttabelle <- merge(FallProDatum,TodeProDatum)

#A00-A04
tabelleA00bisA04 <- subset(tabelle, Altersgruppe == "A00-A04")
tabelleA00bisA04


#Number of cases per reporting date within A00-A04
FallProDatumA00bisA04 <- aggregate(AnzahlFall~Meldedatum, data = tabelleA00bisA04, FUN = sum)
FallProDatumA00bisA04$Meldedatum <- as.Date(FallProDatumA00bisA04$Meldedatum)
FallProDatumA00bisA04

#Number of deaths per reporting date within A00-A04
TodeProDatumA00bisA04 <- aggregate(AnzahlTodesfall~Meldedatum, data = tabelleA00bisA04, FUN = sum)
TodeProDatumA00bisA04$Meldedatum <- as.Date(TodeProDatumA00bisA04$Meldedatum)
TodeProDatumA00bisA04

#Merge
GesamttabelleA00bisA04 <- merge(FallProDatumA00bisA04,TodeProDatumA00bisA04)
GesamttabelleA00bisA04

#A05-A14
tabelleA05bisA14 <- subset(tabelle, Altersgruppe == "A05-A14")
tabelleA05bisA14


#Number of cases per reporting date within A05-A14
FallProDatumA05bisA14 <- aggregate(AnzahlFall~Meldedatum, data = tabelleA05bisA14, FUN = sum)
FallProDatumA05bisA14$Meldedatum <- as.Date(FallProDatumA05bisA14$Meldedatum)
FallProDatumA05bisA14

#Number of deaths per reporting date within A05-A14
TodeProDatumA05bisA14 <- aggregate(AnzahlTodesfall~Meldedatum, data = tabelleA05bisA14, FUN = sum)
TodeProDatumA05bisA14$Meldedatum <- as.Date(TodeProDatumA05bisA14$Meldedatum)
TodeProDatumA05bisA14

#Merge
GesamttabelleA05bisA14 <- merge(FallProDatumA05bisA14,TodeProDatumA05bisA14)
GesamttabelleA05bisA14

#A15-A34
tabelleA15bisA34 <- subset(tabelle, Altersgruppe == "A15-A34")
tabelleA15bisA34


#Number of cases per reporting date within A15-A34
FallProDatumA15bisA34 <- aggregate(AnzahlFall~Meldedatum, data = tabelleA15bisA34, FUN = sum)
FallProDatumA15bisA34$Meldedatum <- as.Date(FallProDatumA15bisA34$Meldedatum)
FallProDatumA15bisA34

#Number of deaths per reporting date within A15-A34
TodeProDatumA15bisA34 <- aggregate(AnzahlTodesfall~Meldedatum, data = tabelleA15bisA34, FUN = sum)
TodeProDatumA15bisA34$Meldedatum <- as.Date(TodeProDatumA15bisA34$Meldedatum)
TodeProDatumA15bisA34

#Merge
GesamttabelleA15bisA34 <- merge(FallProDatumA15bisA34,TodeProDatumA15bisA34)
GesamttabelleA15bisA34

#A35-A59
tabelleA35bisA59 <- subset(tabelle, Altersgruppe == "A35-A59")
tabelleA35bisA59


#Number of cases per reporting date within A35-A59
FallProDatumA35bisA59 <- aggregate(AnzahlFall~Meldedatum, data = tabelleA35bisA59, FUN = sum)
FallProDatumA35bisA59$Meldedatum <- as.Date(FallProDatumA35bisA59$Meldedatum)
FallProDatumA35bisA59

#Number of deaths per reporting date within A35-A59
TodeProDatumA35bisA59 <- aggregate(AnzahlTodesfall~Meldedatum, data = tabelleA35bisA59, FUN = sum)
TodeProDatumA35bisA59$Meldedatum <- as.Date(TodeProDatumA35bisA59$Meldedatum)
TodeProDatumA35bisA59

#Merge
GesamttabelleA35bisA59 <- merge(FallProDatumA35bisA59,TodeProDatumA35bisA59)
GesamttabelleA35bisA59

#A60-A79
tabelleA60bisA79 <- subset(tabelle, Altersgruppe == "A60-A79")
tabelleA60bisA79


#Number of cases per reporting date within A60-A79
FallProDatumA60bisA79 <- aggregate(AnzahlFall~Meldedatum, data = tabelleA60bisA79, FUN = sum)
FallProDatumA60bisA79$Meldedatum <- as.Date(FallProDatumA60bisA79$Meldedatum)
FallProDatumA60bisA79

#Number of deaths per reporting date within A60-A79
TodeProDatumA60bisA79 <- aggregate(AnzahlTodesfall~Meldedatum, data = tabelleA60bisA79, FUN = sum)
TodeProDatumA60bisA79$Meldedatum <- as.Date(TodeProDatumA60bisA79$Meldedatum)
TodeProDatumA60bisA79

#Merge
GesamttabelleA60bisA79 <- merge(FallProDatumA60bisA79,TodeProDatumA60bisA79)
GesamttabelleA60bisA79

#A80+
tabelleA80plus <- subset(tabelle, Altersgruppe == "A80+")
tabelleA80plus


#Number of cases per reporting date within A80+
FallProDatumA80plus <- aggregate(AnzahlFall~Meldedatum, data = tabelleA80plus, FUN = sum)
FallProDatumA80plus$Meldedatum <- as.Date(FallProDatumA80plus$Meldedatum)
FallProDatumA80plus

#Number of deaths per reporting date within A80+
TodeProDatumA80plus <- aggregate(AnzahlTodesfall~Meldedatum, data = tabelleA80plus, FUN = sum)
TodeProDatumA80plus$Meldedatum <- as.Date(TodeProDatumA80plus$Meldedatum)
TodeProDatumA80plus

#Merge
GesamttabelleA80plus <- merge(FallProDatumA80plus,TodeProDatumA80plus)
GesamttabelleA80plus


#Vector age groups
altersgruppenvektor <- unique(tabelle$Altersgruppe)
altersgruppenvektor



##Shiny-App
ui <- fluidPage(
    titlePanel("Grafische Darstellung der vom Robert-Koch-Institut (RKI) 
               bereitgestellten Daten zur SARS-CoV2 Infektionswelle"),
    hr(),
        plotOutput('plot'),
        fluidRow(
            column(3,
            helpText(HTML("<span style='font-size:25px; color: black; font-family: Helvetica'>
            &#10073;&nbsp;&nbsp;Coronafälle</span><br><span style='font-size:25px; color: firebrick;
            font-family: Helvetica'>&#10073;&nbsp;&nbsp;Todesfälle</span><br><span style='font-size:9px; color: grey29; font-family: Helvetica'>Quellen:<br>https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0 (zuletzt besucht am 22.01.2021 20:05 Uhr)<br>https://shiny.rstudio.com/tutorial/ (zuletzt besucht am 23.01.2021 12:14 Uhr)</span>")),
            tags$hr(),
            radioButtons(inputId = 'checkbox', label = 'Merkmal', c('nur Infektionen', 'nur Todesfälle', 'Beides'), 
                         selected = c('Beides')),
            tags$hr()),
            column(4,
            helpText(HTML("Folgende Altersgruppen sind vom RKI vordefiniert:<br>"),
            HTML("0 bis 4 Jahre&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp5 bis 14 Jahre&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp15 bis 34 Jahre<br>
                 35 bis 59 Jahre&nbsp&nbsp&nbsp&nbsp60 bis 79 Jahre&nbsp&nbsp&nbsp&nbsp80+ Jahre<br>Die Altersgruppe \"unbekannt\" wird nicht betrachtet.")),
            helpText("Bitte setzen Sie ein Häkchen, um den Slider zur Auswahl der Altersgruppen zu aktivieren."),
            checkboxInput(inputId = 'unterteilungAltersgruppen', label = 'Unterteilung in Altersgruppen'),
            sliderInput(inputId = 'altersgruppe', label = 'Altersgruppe', min = 0, max = 100, step = 1, value = 0, width = '100%', post = " Jahre"),
            ),
            column(4,
            sliderInput(inputId = 'glaettung', label = 'Glättung der Coronafälle nach LOESS-Verfahren', min = 0, max = 1, step = 0.05, value = 0, width = '100%'),
            tags$hr(),
            sliderInput(inputId = 'glaettung2', label = 'Glättung der Todesfälle nach LOESS-Verfahren', min = 0, max = 1, step = 0.05, value = 0, width = '100%'),
            tags$hr(),
            sliderInput(inputId = 'skalierung', label = 'Skalierung der y-Achse', min = 500, max = 35000, step = 1000, value = 35000,
            width = '100%', post = " Personen", sep = "."))
        )
)

server <- function(input, output, session) {
    output$plot <- renderPlot({
        if (input$glaettung == 0){
            sichtbar <- 1
        }
        else {
            sichtbar <- 0
        }
        
        if (input$glaettung2 == 0){
            sichtbar2 <- 1
        }
        else {
            sichtbar2 <- 0
        } 
        
        if(input$unterteilungAltersgruppen == FALSE){
            if(input$checkbox=='Beides'){
                ggplot(Gesamttabelle, aes(x=Meldedatum))+
                    geom_point(aes(y=AnzahlFall), colour="grey29", size=0.5)+ ##Datenpunkte der Fälle
                    geom_line(aes(y=AnzahlFall), colour="grey29", linetype=sichtbar, size=1)+ ##Graph der Fälle
                    geom_point(aes(y=AnzahlTodesfall), colour="firebrick", size=0.5)+ ##Datenpunkte der Todesfälle
                    geom_line(aes(y=AnzahlTodesfall), colour="firebrick", linetype=sichtbar2, size=1)+  ##Graph der Todesfälle
                    labs(x="Meldedatum", y="Anzahl der gemeldeten Fälle", title="Gemeldete Coronafälle und Todesfälle in Deutschland aller Altersgruppe")+ ##Beschriftungen
                    theme(plot.title = element_text(hjust = 0.5))+ ##Titel zentrieren
                    scale_x_date(breaks = "month", date_labels = "%b %Y")+
                    scale_y_continuous(limits = c(0, input$skalierung))+
                    geom_smooth(aes(y=AnzahlFall), method="loess", span = input$glaettung, col = "grey29")+
                    geom_smooth(aes(y=AnzahlTodesfall), method="loess", span = input$glaettung2, col = "firebrick")
            }
            else if(input$checkbox=='nur Infektionen'){
                ggplot(Gesamttabelle, aes(x=Meldedatum))+
                    geom_point(aes(y=AnzahlFall), colour="grey29", size=0.5)+ ##Datenpunkte der Fälle
                    geom_line(aes(y=AnzahlFall), colour="grey29", linetype=sichtbar, size=1)+ ##Graph der Fälle
                    labs(x="Meldedatum", y="Anzahl der gemeldeten Fälle", title="Gemeldete Coronafälle in Deutschland aller Altersgruppen")+ ##Beschriftungen
                    theme(plot.title = element_text(hjust = 0.5))+ ##Titel zentrieren
                    scale_x_date(breaks = "month", date_labels = "%b %Y")+
                    scale_y_continuous(limits = c(0, input$skalierung))+
                    theme(panel.background = element_rect())+
                    geom_smooth(aes(y=AnzahlFall), method="loess", span = input$glaettung, col = "grey29")
            }
            else if(input$checkbox=='nur Todesfälle'){
                ggplot(Gesamttabelle, aes(x=Meldedatum))+
                    geom_point(aes(y=AnzahlTodesfall), colour="firebrick", size=0.5)+ ##Datenpunkte der Todesfälle
                    geom_line(aes(y=AnzahlTodesfall), colour="firebrick", linetype=sichtbar2, size=1)+  ##Graph der Todesfälle
                    labs(x="Meldedatum", y="Anzahl der gemeldeten Fälle", title="Gemeldete Todesfälle in Deutschland aller Altersgruppen")+ ##Beschriftungen
                    theme(plot.title = element_text(hjust = 0.5))+ ##Titel zentrieren
                    scale_x_date(breaks = "month", date_labels = "%b %Y")+
                    scale_y_continuous(limits = c(0, input$skalierung))+
                    geom_smooth(aes(y=AnzahlTodesfall), method="loess", span = input$glaettung2, col = "firebrick")
            }
            else {
                print("Fehler")
            }
        }
        else {
        if(input$altersgruppe >= 0 && input$altersgruppe <= 4){
            
            if(input$checkbox=='Beides'){
                ggplot(GesamttabelleA00bisA04, aes(x=Meldedatum))+
                    geom_point(aes(y=AnzahlFall), colour="grey29", size=0.5)+ ##Datenpunkte der Fälle
                    geom_line(aes(y=AnzahlFall), colour="grey29", linetype=sichtbar, size=1)+ ##Graph der Fälle
                    geom_point(aes(y=AnzahlTodesfall), colour="firebrick", size=0.5)+ ##Datenpunkte der Todesfälle
                    geom_line(aes(y=AnzahlTodesfall), colour="firebrick", linetype=sichtbar2, size=1)+  ##Graph der Todesfälle
                    labs(x="Meldedatum", y="Anzahl der gemeldeten Fälle", title="Gemeldete Coronafälle und Todesfälle in Deutschland, Altersgruppe 0-4 Jahre")+ ##Beschriftungen
                    theme(plot.title = element_text(hjust = 0.5))+ ##Titel zentrieren
                    scale_x_date(breaks = "month", date_labels = "%b %Y")+
                    scale_y_continuous(limits = c(0, input$skalierung))+
                    geom_smooth(aes(y=AnzahlFall), method="loess", span = input$glaettung, col = "grey29")+
                    geom_smooth(aes(y=AnzahlTodesfall), method="loess", span = input$glaettung2, col = "firebrick")
            }
            else if(input$checkbox=='nur Infektionen'){
                ggplot(GesamttabelleA00bisA04, aes(x=Meldedatum))+
                    geom_point(aes(y=AnzahlFall), colour="grey29", size=0.5)+ ##Datenpunkte der Fälle
                    geom_line(aes(y=AnzahlFall), colour="grey29", linetype=sichtbar, size=1)+ ##Graph der Fälle
                    labs(x="Meldedatum", y="Anzahl der gemeldeten Fälle", title="Gemeldete Coronafälle in Deutschland, Altersgruppe 0-4 Jahre")+ ##Beschriftungen
                    theme(plot.title = element_text(hjust = 0.5))+ ##Titel zentrieren
                    scale_x_date(breaks = "month", date_labels = "%b %Y")+
                    scale_y_continuous(limits = c(0, input$skalierung))+
                    theme(panel.background = element_rect())+
                    geom_smooth(aes(y=AnzahlFall), method="loess", span = input$glaettung, col = "grey29")
            }
            else if(input$checkbox=='nur Todesfälle'){
                ggplot(GesamttabelleA00bisA04, aes(x=Meldedatum))+
                    geom_point(aes(y=AnzahlTodesfall), colour="firebrick", size=0.5)+ ##Datenpunkte der Todesfälle
                    geom_line(aes(y=AnzahlTodesfall), colour="firebrick", linetype=sichtbar2, size=1)+  ##Graph der Todesfälle
                    labs(x="Meldedatum", y="Anzahl der gemeldeten Fälle", title="Gemeldete Todesfälle in Deutschland, Altersgruppe 0-4 Jahre")+ ##Beschriftungen
                    theme(plot.title = element_text(hjust = 0.5))+ ##Titel zentrieren
                    scale_x_date(breaks = "month", date_labels = "%b %Y")+
                    scale_y_continuous(limits = c(0, input$skalierung))+
                    geom_smooth(aes(y=AnzahlTodesfall), method="loess", span = input$glaettung2, col = "firebrick")
            }
            else {
                print("Fehler")
            }
        }
        else if(input$altersgruppe >= 5 && input$altersgruppe <= 14){
            
            if(input$checkbox=='Beides'){
                ggplot(GesamttabelleA05bisA14, aes(x=Meldedatum))+
                    geom_point(aes(y=AnzahlFall), colour="grey29", size=0.5)+ ##Datenpunkte der Fälle
                    geom_line(aes(y=AnzahlFall), colour="grey29", linetype=sichtbar, size=1)+ ##Graph der Fälle
                    geom_point(aes(y=AnzahlTodesfall), colour="firebrick", size=0.5)+ ##Datenpunkte der Todesfälle
                    geom_line(aes(y=AnzahlTodesfall), colour="firebrick", linetype=sichtbar2, size=1)+  ##Graph der Todesfälle
                    labs(x="Meldedatum", y="Anzahl der gemeldeten Fälle", title="Gemeldete Coronafälle und Todesfälle in Deutschland, Altersgruppe 5-14 Jahre")+ ##Beschriftungen
                    theme(plot.title = element_text(hjust = 0.5))+ ##Titel zentrieren
                    scale_x_date(breaks = "month", date_labels = "%b %Y")+
                    scale_y_continuous(limits = c(0, input$skalierung))+
                    geom_smooth(aes(y=AnzahlFall), method="loess", span = input$glaettung, col = "grey29")+
                    geom_smooth(aes(y=AnzahlTodesfall), method="loess", span = input$glaettung2, col = "firebrick")
                
            }
            else if(input$checkbox=='nur Infektionen'){
                ggplot(GesamttabelleA05bisA14, aes(x=Meldedatum))+
                    geom_point(aes(y=AnzahlFall), colour="grey29", size=0.5)+ ##Datenpunkte der Fälle
                    geom_line(aes(y=AnzahlFall), colour="grey29", linetype=sichtbar, size=1)+ ##Graph der Fälle
                    labs(x="Meldedatum", y="Anzahl der gemeldeten Fälle", title="Gemeldete Coronafälle in Deutschland, Altersgruppe 5-14 Jahre")+ ##Beschriftungen
                    theme(plot.title = element_text(hjust = 0.5))+ ##Titel zentrieren
                    scale_x_date(breaks = "month", date_labels = "%b %Y")+
                    scale_y_continuous(limits = c(0, input$skalierung))+
                    theme(panel.background = element_rect())+
                    geom_smooth(aes(y=AnzahlFall), method="loess", span = input$glaettung, col = "grey29")
            }
            else if(input$checkbox=='nur Todesfälle'){
                ggplot(GesamttabelleA05bisA14, aes(x=Meldedatum))+
                    geom_point(aes(y=AnzahlTodesfall), colour="firebrick", size=0.5)+ ##Datenpunkte der Todesfälle
                    geom_line(aes(y=AnzahlTodesfall), colour="firebrick", linetype=sichtbar2, size=1)+  ##Graph der Todesfälle
                    labs(x="Meldedatum", y="Anzahl der gemeldeten Fälle", title="Gemeldete Todesfälle in Deutschland, Altersgruppe 5-14 Jahre")+ ##Beschriftungen
                    theme(plot.title = element_text(hjust = 0.5))+ ##Titel zentrieren
                    scale_x_date(breaks = "month", date_labels = "%b %Y")+
                    scale_y_continuous(limits = c(0, input$skalierung))+
                    geom_smooth(aes(y=AnzahlTodesfall), method="loess", span = input$glaettung2, col = "firebrick")
            }
            else {
                print("Fehler")
            }
        }else if(input$altersgruppe >= 15 && input$altersgruppe <= 34){
            
            if(input$checkbox=='Beides'){
                ggplot(GesamttabelleA15bisA34, aes(x=Meldedatum))+
                    geom_point(aes(y=AnzahlFall), colour="grey29", size=0.5)+ ##Datenpunkte der Fälle
                    geom_line(aes(y=AnzahlFall), colour="grey29", linetype=sichtbar, size=1)+ ##Graph der Fälle
                    geom_point(aes(y=AnzahlTodesfall), colour="firebrick", size=0.5)+ ##Datenpunkte der Todesfälle
                    geom_line(aes(y=AnzahlTodesfall), colour="firebrick", linetype=sichtbar2, size=1)+  ##Graph der Todesfälle
                    labs(x="Meldedatum", y="Anzahl der gemeldeten Fälle", title="Gemeldete Coronafälle und Todesfälle in Deutschland, Altersgruppe 15-34 Jahre")+ ##Beschriftungen
                    theme(plot.title = element_text(hjust = 0.5))+ ##Titel zentrieren
                    scale_x_date(breaks = "month", date_labels = "%b %Y")+
                    scale_y_continuous(limits = c(0, input$skalierung))+
                    geom_smooth(aes(y=AnzahlFall), method="loess", span = input$glaettung, col = "grey29")+
                    geom_smooth(aes(y=AnzahlTodesfall), method="loess", span = input$glaettung2, col = "firebrick")
            }
            else if(input$checkbox=='nur Infektionen'){
                ggplot(GesamttabelleA15bisA34, aes(x=Meldedatum))+
                    geom_point(aes(y=AnzahlFall), colour="grey29", size=0.5)+ ##Datenpunkte der Fälle
                    geom_line(aes(y=AnzahlFall), colour="grey29", linetype=sichtbar, size=1)+ ##Graph der Fälle
                    labs(x="Meldedatum", y="Anzahl der gemeldeten Fälle", title="Gemeldete Coronafälle in Deutschland, Altersgruppe 15-34 Jahre")+ ##Beschriftungen
                    theme(plot.title = element_text(hjust = 0.5))+ ##Titel zentrieren
                    scale_x_date(breaks = "month", date_labels = "%b %Y")+
                    scale_y_continuous(limits = c(0, input$skalierung))+
                    theme(panel.background = element_rect())+
                    geom_smooth(aes(y=AnzahlFall), method="loess", span = input$glaettung, col = "grey29")
            }
            else if(input$checkbox=='nur Todesfälle'){
                ggplot(GesamttabelleA15bisA34, aes(x=Meldedatum))+
                    geom_point(aes(y=AnzahlTodesfall), colour="firebrick", size=0.5)+ ##Datenpunkte der Todesfälle
                    geom_line(aes(y=AnzahlTodesfall), colour="firebrick", linetype=sichtbar2, size=1)+  ##Graph der Todesfälle
                    labs(x="Meldedatum", y="Anzahl der gemeldeten Fälle", title="Gemeldete Todesfälle in Deutschland, Altersgruppe 15-34 Jahre")+ ##Beschriftungen
                    theme(plot.title = element_text(hjust = 0.5))+ ##Titel zentrieren
                    scale_x_date(breaks = "month", date_labels = "%b %Y")+
                    scale_y_continuous(limits = c(0, input$skalierung))+
                    geom_smooth(aes(y=AnzahlTodesfall), method="loess", span = input$glaettung2, col = "firebrick")
            }
            else {
                print("Fehler")
            }
        }else if(input$altersgruppe >= 35 && input$altersgruppe <= 59){
            
            if(input$checkbox=='Beides'){
                ggplot(GesamttabelleA35bisA59, aes(x=Meldedatum))+
                    geom_point(aes(y=AnzahlFall), colour="grey29", size=0.5)+ ##Datenpunkte der Fälle
                    geom_line(aes(y=AnzahlFall), colour="grey29", linetype=sichtbar, size=1)+ ##Graph der Fälle
                    geom_point(aes(y=AnzahlTodesfall), colour="firebrick", size=0.5)+ ##Datenpunkte der Todesfälle
                    geom_line(aes(y=AnzahlTodesfall), colour="firebrick", linetype=sichtbar2, size=1)+  ##Graph der Todesfälle
                    labs(x="Meldedatum", y="Anzahl der gemeldeten Fälle", title="Gemeldete Coronafälle und Todesfälle in Deutschland, Altersgruppe 35-59 Jahre")+ ##Beschriftungen
                    theme(plot.title = element_text(hjust = 0.5))+ ##Titel zentrieren
                    scale_x_date(breaks = "month", date_labels = "%b %Y")+
                    scale_y_continuous(limits = c(0, input$skalierung))+
                    geom_smooth(aes(y=AnzahlFall), method="loess", span = input$glaettung, col = "grey29")+
                    geom_smooth(aes(y=AnzahlTodesfall), method="loess", span = input$glaettung2, col = "firebrick")
            }
            else if(input$checkbox=='nur Infektionen'){
                ggplot(GesamttabelleA35bisA59, aes(x=Meldedatum))+
                    geom_point(aes(y=AnzahlFall), colour="grey29", size=0.5)+ ##Datenpunkte der Fälle
                    geom_line(aes(y=AnzahlFall), colour="grey29", linetype=sichtbar, size=1)+ ##Graph der Fälle
                    labs(x="Meldedatum", y="Anzahl der gemeldeten Fälle", title="Gemeldete Coronafälle in Deutschland, Altersgruppe 35-59 Jahre")+ ##Beschriftungen
                    theme(plot.title = element_text(hjust = 0.5))+ ##Titel zentrieren
                    scale_x_date(breaks = "month", date_labels = "%b %Y")+
                    scale_y_continuous(limits = c(0, input$skalierung))+
                    theme(panel.background = element_rect())+
                    geom_smooth(aes(y=AnzahlFall), method="loess", span = input$glaettung, col = "grey29")
            }
            else if(input$checkbox=='nur Todesfälle'){
                ggplot(GesamttabelleA35bisA59, aes(x=Meldedatum))+
                    geom_point(aes(y=AnzahlTodesfall), colour="firebrick", size=0.5)+ ##Datenpunkte der Todesfälle
                    geom_line(aes(y=AnzahlTodesfall), colour="firebrick", linetype=sichtbar2, size=1)+  ##Graph der Todesfälle
                    labs(x="Meldedatum", y="Anzahl der gemeldeten Fälle", title="Gemeldete Todesfälle in Deutschland, Altersgruppe 35-59 Jahre")+ ##Beschriftungen
                    theme(plot.title = element_text(hjust = 0.5))+ ##Titel zentrieren
                    scale_x_date(breaks = "month", date_labels = "%b %Y")+
                    scale_y_continuous(limits = c(0, input$skalierung))+
                    geom_smooth(aes(y=AnzahlTodesfall), method="loess", span = input$glaettung2, col = "firebrick")
            }
            else {
                print("Fehler")
            }
        }else if(input$altersgruppe >= 60 && input$altersgruppe <= 79){
            
            if(input$checkbox=='Beides'){
                ggplot(GesamttabelleA60bisA79, aes(x=Meldedatum))+
                    geom_point(aes(y=AnzahlFall), colour="grey29", size=0.5)+ ##Datenpunkte der Fälle
                    geom_line(aes(y=AnzahlFall), colour="grey29", linetype=sichtbar, size=1)+ ##Graph der Fälle
                    geom_point(aes(y=AnzahlTodesfall), colour="firebrick", size=0.5)+ ##Datenpunkte der Todesfälle
                    geom_line(aes(y=AnzahlTodesfall), colour="firebrick", linetype=sichtbar2, size=1)+  ##Graph der Todesfälle
                    labs(x="Meldedatum", y="Anzahl der gemeldeten Fälle", title="Gemeldete Coronafälle und Todesfälle in Deutschland, Altersgruppe 60-79 Jahre")+ ##Beschriftungen
                    theme(plot.title = element_text(hjust = 0.5))+ ##Titel zentrieren
                    scale_x_date(breaks = "month", date_labels = "%b %Y")+
                    scale_y_continuous(limits = c(0, input$skalierung))+
                    geom_smooth(aes(y=AnzahlFall), method="loess", span = input$glaettung, col = "grey29")+
                    geom_smooth(aes(y=AnzahlTodesfall), method="loess", span = input$glaettung2, col = "firebrick")
            }
            else if(input$checkbox=='nur Infektionen'){
                ggplot(GesamttabelleA60bisA79, aes(x=Meldedatum))+
                    geom_point(aes(y=AnzahlFall), colour="grey29", size=0.5)+ ##Datenpunkte der Fälle
                    geom_line(aes(y=AnzahlFall), colour="grey29", linetype=sichtbar, size=1)+ ##Graph der Fälle
                    labs(x="Meldedatum", y="Anzahl der gemeldeten Fälle", title="Gemeldete Coronafälle in Deutschland, Altersgruppe 60-79 Jahre")+ ##Beschriftungen
                    theme(plot.title = element_text(hjust = 0.5))+ ##Titel zentrieren
                    scale_x_date(breaks = "month", date_labels = "%b %Y")+
                    scale_y_continuous(limits = c(0, input$skalierung))+
                    theme(panel.background = element_rect())+
                    geom_smooth(aes(y=AnzahlFall), method="loess", span = input$glaettung, col = "grey29")
            }
            else if(input$checkbox=='nur Todesfälle'){
                ggplot(GesamttabelleA60bisA79, aes(x=Meldedatum))+
                    geom_point(aes(y=AnzahlTodesfall), colour="firebrick", size=0.5)+ ##Datenpunkte der Todesfälle
                    geom_line(aes(y=AnzahlTodesfall), colour="firebrick", linetype=sichtbar2, size=1)+  ##Graph der Todesfälle
                    labs(x="Meldedatum", y="Anzahl der gemeldeten Fälle", title="Gemeldete Todesfälle in Deutschland, Altersgruppe 60-79 Jahre")+ ##Beschriftungen
                    theme(plot.title = element_text(hjust = 0.5))+ ##Titel zentrieren
                    scale_x_date(breaks = "month", date_labels = "%b %Y")+
                    scale_y_continuous(limits = c(0, input$skalierung))+
                    geom_smooth(aes(y=AnzahlTodesfall), method="loess", span = input$glaettung2, col = "firebrick")
            }
            else {
                print("Fehler")
            }
        }else if(input$altersgruppe >= 80){
            
            if(input$checkbox=='Beides'){
                ggplot(GesamttabelleA80plus, aes(x=Meldedatum))+
                    geom_point(aes(y=AnzahlFall), colour="grey29", size=0.5)+ ##Datenpunkte der Fälle
                    geom_line(aes(y=AnzahlFall), colour="grey29", linetype=sichtbar, size=1)+ ##Graph der Fälle
                    geom_point(aes(y=AnzahlTodesfall), colour="firebrick", size=0.5)+ ##Datenpunkte der Todesfälle
                    geom_line(aes(y=AnzahlTodesfall), colour="firebrick", linetype=sichtbar2, size=1)+  ##Graph der Todesfälle
                    labs(x="Meldedatum", y="Anzahl der gemeldeten Fälle", title="Gemeldete Coronafälle und Todesfälle in Deutschland, Altersgruppe 80+ Jahre")+ ##Beschriftungen
                    theme(plot.title = element_text(hjust = 0.5))+ ##Titel zentrieren
                    scale_x_date(breaks = "month", date_labels = "%b %Y")+
                    scale_y_continuous(limits = c(0, input$skalierung))+
                    geom_smooth(aes(y=AnzahlFall), method="loess", span = input$glaettung, col = "grey29")+
                    geom_smooth(aes(y=AnzahlTodesfall), method="loess", span = input$glaettung2, col = "firebrick")
            }
            else if(input$checkbox=='nur Infektionen'){
                ggplot(GesamttabelleA80plus, aes(x=Meldedatum))+
                    geom_point(aes(y=AnzahlFall), colour="grey29", size=0.5)+ ##Datenpunkte der Fälle
                    geom_line(aes(y=AnzahlFall), colour="grey29", linetype=sichtbar, size=1)+ ##Graph der Fälle
                    labs(x="Meldedatum", y="Anzahl der gemeldeten Fälle", title="Gemeldete Coronafälle in Deutschland, Altersgruppe 80+ Jahre")+ ##Beschriftungen
                    theme(plot.title = element_text(hjust = 0.5))+ ##Titel zentrieren
                    scale_x_date(breaks = "month", date_labels = "%b %Y")+
                    scale_y_continuous(limits = c(0, input$skalierung))+
                    theme(panel.background = element_rect())+
                    geom_smooth(aes(y=AnzahlFall), method="loess", span = input$glaettung, col = "grey29")
            }
            else if(input$checkbox=='nur Todesfälle'){
                ggplot(GesamttabelleA80plus, aes(x=Meldedatum))+
                    geom_point(aes(y=AnzahlTodesfall), colour="firebrick", size=0.5)+ ##Datenpunkte der Todesfälle
                    geom_line(aes(y=AnzahlTodesfall), colour="firebrick", linetype=sichtbar2, size=1)+  ##Graph der Todesfälle
                    labs(x="Meldedatum", y="Anzahl der gemeldeten Fälle", title="Gemeldete Todesfälle in Deutschland, Altersgruppe 80+ Jahre")+ ##Beschriftungen
                    theme(plot.title = element_text(hjust = 0.5))+ ##Titel zentrieren
                    scale_x_date(breaks = "month", date_labels = "%b %Y")+
                    scale_y_continuous(limits = c(0, input$skalierung))+
                    geom_smooth(aes(y=AnzahlTodesfall), method="loess", span = input$glaettung2, col = "firebrick")
            }
            else {
                print("Fehler")
            }
        }
        }
    })
}
shinyApp(ui = ui, server = server)