# libraries
library(shiny)
library(dplyr)
library(shinydashboard)
library(DT)
library(tidyverse)
library(devtools)
library(ggplot2)
library(lubridate)
library(caret)



# Preparation des données -------------------------------------------------


library(readr)
bilan_electrique_transpose <- readRDS('data/conso_data.rds')
temp <- readRDS('data2/temperatures2.rds')

# bilan_electrique_transpose <- read_delim("C:/Users/Camille/Documents/Projet_shiny/bilan-electrique-transpose.csv", 
#                                          ";", escape_double = FALSE, trim_ws = TRUE)

# renommer les variables conso

colnames(bilan_electrique_transpose) <- c("jour","categorie_client","puissance")
bilan <- bilan_electrique_transpose[order(bilan_electrique_transpose$jour),]
bilan$categorie_client=as.factor(bilan$categorie_client)

# renommer les variables temp

temp$Horodate<-as.Date(temp$Horodate, format = "%d/%m/%Y")
class(temp$Horodate)
temp$heure<-as.character(temp$heure)
class(temp$heure)
temp<-temp%>%filter(heure==as.character("12:00:00"))

temperature_ord <- temp[order(temp$Horodate),]

# ## preparation pour regression
# 
# bilan<-bilan%>%filter(jour>=as.Date("2015-12-18"))
# bilan<-bilan%>%filter(categorie_client=="Entreprises")
# bilan<-bilan%>%filter(jour<=as.Date("2016-12-18"))
# 
# temperature_ord<-temperature_ord%>%filter(Horodate>=as.Date("2015-12-18"))
# temperature_ord<-temperature_ord%>%filter(Horodate<=as.Date("2016-12-18"))
# 
# temperature_only<-temperature_ord%>%select(Horodate,temperature)
# colnames(temperature_only) <- c("jour","temperature")
# join<- left_join(bilan, temperature_only)

# ## preparation pour foret

bilan2<-bilan%>%filter(jour>=as.Date("2015-12-18"))
bilan2<-bilan2%>%filter(categorie_client=="Entreprises")
bilan2<-bilan2%>%filter(jour<=as.Date("2016-12-18"))

temperature_ord<-temperature_ord%>%filter(Horodate>=as.Date("2015-12-18"))
temperature_ord<-temperature_ord%>%filter(Horodate<=as.Date("2016-12-18"))

temperature_only<-temperature_ord%>%select(Horodate,temperature)
colnames(temperature_only) <- c("jour","temperature")
join<- left_join(bilan2, temperature_only)


temperature_jfer <- mutate(join,
                           jour_ferier = case_when(
                               day(jour) == 25 & month(jour)==12 ~ 1,
                               day(jour) == 31 & month(jour)==12 ~ 1,
                               day(jour) == 11 & month(jour)==11 ~ 1,
                               day(jour) == 14 & month(jour)==07 ~ 1,
                               day(jour) == 01 & month(jour)==01 ~ 1,
                               day(jour) == 15 & month(jour)==08 ~ 1,
                               day(jour) == 08 & month(jour)==05 ~ 1,
                               day(jour) == 01 & month(jour)==05 ~ 1,
                               day(jour) == 01 & month(jour)==11 ~ 1,
                               jour==as.Date("2016-05-16")~1,
                               jour==as.Date("2016-05-15")~1,
                               jour==as.Date("2016-05-05")~1,
                               jour==as.Date("2016-03-28")~1,
                               jour==as.Date("2016-03-27")~1,
                               jour==as.Date("2015-05-25")~1,
                               jour==as.Date("2015-05-24")~1,
                               jour==as.Date("2015-05-14")~1,
                               jour==as.Date("2015-04-06")~1,
                               jour==as.Date("2015-04-05")~1,
                               jour==as.Date("2017-06-05")~1,
                               jour==as.Date("2017-06-04")~1,
                               jour==as.Date("2017-05-25")~1,
                               jour==as.Date("2017-04-17")~1,
                               jour==as.Date("2017-04-16")~1,
                               jour==as.Date("2018-05-21")~1,
                               jour==as.Date("2018-05-20")~1,
                               jour==as.Date("2018-05-10")~1,
                               jour==as.Date("2018-04-02")~1,
                               jour==as.Date("2018-04-01")~1,
                               jour==as.Date("2019-06-10")~1,
                               jour==as.Date("2019-06-09")~1,
                               jour==as.Date("2019-05-30")~1,
                               jour==as.Date("2019-04-22")~1,
                               jour==as.Date("2019-04-21")~1,
                               jour==as.Date("2020-06-01")~1,
                               jour==as.Date("2020-05-31")~1,
                               jour==as.Date("2020-05-21")~1,
                               jour==as.Date("2020-04-13")~1,
                               jour==as.Date("2020-04-12")~1,
                               TRUE~0))

temp_jfer_pos <- mutate(temperature_jfer,
                        position_semaine = case_when(
                            wday(jour) >= 2 & wday(jour)<=6 ~ "de lundi a vendredi",
                            wday(jour) == 7 | wday(jour)==1 ~ "week-end"))

temp_jfer_pos_vac <- mutate(temp_jfer_pos,
                        vacance = case_when(
                            jour>=as.Date("2015-12-19") & jour <=as.Date("2016-01-04") ~1,
                            jour>=as.Date("2016-02-06") & jour <=as.Date("2016-02-13") ~1/3,
                            jour>as.Date("2016-02-13") & jour <=as.Date("2016-02-22") ~2/3,
                            jour>as.Date("2016-02-22") & jour <=as.Date("2016-02-29") ~2/3,
                            jour>as.Date("2016-02-29") & jour <=as.Date("2016-03-07") ~1/3,
                            jour>=as.Date("2016-04-02") & jour <=as.Date("2016-04-09") ~1/3,
                            jour>as.Date("2016-04-09") & jour <=as.Date("2016-04-16") ~2/3,
                            jour>as.Date("2016-04-16") & jour <=as.Date("2016-04-25") ~2/3,
                            jour>as.Date("2016-04-25") & jour <=as.Date("2016-05-02") ~1/3,
                            jour>as.Date("2016-07-05") & jour <=as.Date("2016-09-01") ~1,
                            jour>as.Date("2016-10-19") & jour <=as.Date("2016-11-03") ~1,
                            jour>as.Date("2016-12-17") & jour <=as.Date("2017-01-03") ~1,
                            jour>as.Date("2017-02-04") & jour <=as.Date("2017-02-11") ~1/3,
                            jour>as.Date("2017-02-11") & jour <=as.Date("2017-02-18") ~2/3,
                            jour>as.Date("2017-02-18") & jour <=as.Date("2017-02-27") ~2/3,
                            jour>as.Date("2017-02-27") & jour <=as.Date("2017-03-06") ~1/3,
                            jour>=as.Date("2017-04-01") & jour <=as.Date("2017-04-08") ~1/3,
                            jour>as.Date("2017-04-08") & jour <=as.Date("2017-04-24") ~2/3,
                            jour>as.Date("2017-04-24") & jour <=as.Date("2017-05-02") ~1/3,
                            jour>as.Date("2017-07-08") & jour <=as.Date("2017-09-04") ~1,
                            jour>as.Date("2017-10-21") & jour <=as.Date("2017-11-06") ~1,
                            jour>as.Date("2017-12-23") & jour <=as.Date("2018-01-08") ~1,
                            jour>as.Date("2018-02-10") & jour <=as.Date("2018-02-17") ~1/3,
                            jour>as.Date("2018-02-17") & jour <=as.Date("2018-03-05") ~2/3,
                            jour>as.Date("2018-03-05") & jour <=as.Date("2018-03-12") ~1/3,
                            jour>as.Date("2018-04-07") & jour <=as.Date("2018-04-14") ~1/3,
                            jour>as.Date("2018-04-14") & jour <=as.Date("2018-04-30") ~2/3,
                            jour>as.Date("2018-04-30") & jour <=as.Date("2018-05-07") ~1/3,
                            jour>as.Date("2018-07-07") & jour <=as.Date("2018-09-03") ~1,
                            jour>as.Date("2018-10-20") & jour <=as.Date("2018-11-05") ~1,
                            jour>as.Date("2018-12-22") & jour <=as.Date("2019-01-07") ~1,
                            jour>as.Date("2019-02-09") & jour <=as.Date("2019-02-16") ~1/3,
                            jour>as.Date("2019-02-16") & jour <=as.Date("2019-03-04") ~2/3,
                            jour>as.Date("2019-03-04") & jour <=as.Date("2019-03-11") ~1/3,
                            jour>as.Date("2019-04-06") & jour <=as.Date("2019-04-13") ~1/3,
                            jour>as.Date("2019-04-13") & jour <=as.Date("2019-04-29") ~2/3,
                            jour>as.Date("2019-04-29") & jour <=as.Date("2019-05-06") ~1/3,
                            jour>as.Date("2019-07-06") & jour <=as.Date("2019-09-02") ~1,
                            jour>as.Date("2019-10-19") & jour <=as.Date("2019-11-04") ~1,
                            jour>as.Date("2019-12-21") & jour <=as.Date("2020-01-06") ~1,
                            jour>as.Date("2020-02-08") & jour <=as.Date("2020-02-15") ~1/3,
                            jour>as.Date("2020-02-15") & jour <=as.Date("2020-03-02") ~2/3,
                            jour>as.Date("2020-03-02") & jour <=as.Date("2020-03-09") ~1/3,
                            jour>as.Date("2020-04-04") & jour <=as.Date("2020-04-11") ~1/3,
                            jour>as.Date("2020-04-11") & jour <=as.Date("2020-04-27") ~2/3,
                            jour>as.Date("2020-04-27") & jour <=as.Date("2020-05-04") ~1/3,
                            jour>as.Date("2020-07-04") & jour <=as.Date("2020-09-01") ~1,
                            jour>as.Date("2020-10-02") & jour <=as.Date("2020-11-02") ~1,
                            TRUE~0))

temp_jfer_pos_vac$vacance<-as.factor(temp_jfer_pos_vac$vacance)
temp_jfer_pos_vac$jour_ferier<-as.factor(temp_jfer_pos_vac$jour_ferier)
temp_jfer_pos_vac$position_semaine<-as.factor(temp_jfer_pos_vac$position_semaine)

class(temp_jfer_pos_vac$vacance)
class(temp_jfer_pos_vac$puissance)

# set.seed(1234)
# param<-data.frame(mtry=4) # 12 covariables
# ctrl<-trainControl(method='cv',number=5,savePredictions=T)
# model_rf2<-train(puissance~.-categorie_client-jour,data=temp_jfer_pos_vac,method='rf',trControl=ctrl,tuneGrid=param,importance=T)
# model_rf2
# print(model_rf2$bestTune)
# model_rf2$pred
# n<-predict(model_rf2)
# class(predict(model_rf2))
# n<-c(n)
# class(n)
# plot(n, type="l")
# plot(varImp(model_rf2))

set.seed(1234)
ctrl<-trainControl(method="cv",number=5)
param=expand.grid(k=seq(2,100,by=1))
model_knn<-train(puissance~.-categorie_client-jour,data=temp_jfer_pos_vac,method='knn',trControl=ctrl,
                 preProcess=c("center","scale"),tuneGrid=param)
model_knn
a=predict(model_knn)
plot(a)
plot(temp_jfer_pos_vac$puissance)



# ui ----------------------------------------------------------------------


# Define UI for application that draws a histogram
ui <- navbarPage(
    'Impact du covid sur les consommations électriques',
    
    tabPanel('Mon département',
             id = 'departements',
             
             
             
             sidebarLayout(
                 sidebarPanel(
                     
                     selectInput("segment",
                                 "Choisissez votre segment:",
                                 choices = levels(bilan$categorie_client),
                                 selected = 'Doubs'),
                     sliderInput("obs",
                                 "date finale:",
                                 min = as.Date("2020-03-17","%Y-%m-%d"),
                                 max = as.Date("2020-11-20","%Y-%m-%d"),
                                 value=as.Date("2020-03-21"),timeFormat="%Y-%m-%d"),
                     sliderInput("obs1",
                                 "date de début:",
                                 min = as.Date("2020-03-17","%Y-%m-%d"),
                                 max = as.Date("2020-11-20","%Y-%m-%d"),
                                 value=as.Date("2020-04-21"),timeFormat="%Y-%m-%d"),
                     valueBoxOutput("progressBox")
                 ),
                 
                 
                 mainPanel(
                     ##affichage du nom du departement
                     h3(textOutput('nom_segment')),
                     
                     
                     ####TODO: remplacer par la table par un datatable 
                     plotOutput('courbe_realise'),
                     tableOutput('ma_table'),
                     downloadLink('downloadData', 'telecharger')
                     
                     
                     
                 )
             )
             
    )
)



# Server ------------------------------------------------------------------


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$nom_segment <- renderText({
        input$segment
    })
    
    
    filtre <- reactive({
        bilan %>% 
            filter(categorie_client == input$segment) %>%
            filter(jour < input$obs)
    })
    
    get_somme <- reactive({
        
        recupere_donnee <- bilan %>% 
            filter(categorie_client == input$segment) %>%
            filter(jour < input$obs)
        somme <- sum(recupere_donnee$puissance)
        
        somme
        
        
    })
    
    output$courbe_realise <- renderPlot({
        
        df <- filtre() %>%
            select(jour, puissance)%>%
            tidyr::pivot_longer(-c("jour"))%>%
            filter(jour > as.Date ("2020-03-15"))
        
        
        fig= ggplot(df) +
            aes(y  = value, x = jour, color = name)+
            geom_line()
        
        fig
        
    })
    output$ma_table <- renderTable({
        out <-  filtre()%>%
            filter(jour > as.Date ("2020-03-15"))
        print(out)
        out
    } )
    
    
    
    output$progressBox <- renderValueBox({
        valueBox("somme consommation reelle", get_somme()
        )
    })
    
    
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep=";")
        },
        content = function(file) {
            write.csv(filtre(), file)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
